import Control.Monad
import Data.Char (isAlphaNum, isDigit, isLetter, isPunctuation)
import Data.List (foldl')
import Text.ParserCombinators.ReadP
import Text.Printf (printf)

data FSItem = File String Int | Dir String [FSItem]
    deriving (Show)

data Location = Root | Up | Down String
    deriving (Show)
data Cmd = ChangeDir Location | ListDir [FSItem]
    deriving (Show)

-- http://learnyouahaskell.com/zippers
-- parent dir, items before, items after
data FSContext = FSContext String [FSItem] [FSItem]
    deriving (Show)

type FSZipper = (FSItem, [FSContext])

newline :: ReadP Char
newline = char '\n'

parseInt :: ReadP Int
parseInt = read <$> munch1 isDigit

parseLocation :: ReadP Location
parseLocation = do
    choice
        [ Root <$ char '/'
        , Up <$ string ".."
        , Down <$> munch1 isLetter
        ]

parseChangeDir :: ReadP Cmd
parseChangeDir = ChangeDir <$> (string "$ cd " *> parseLocation)

isFileName = liftM2 (||) isAlphaNum isPunctuation

parseFile :: ReadP FSItem
parseFile = flip File <$> parseInt <*> (char ' ' *> many1 (satisfy isFileName))

parseDir :: ReadP FSItem
parseDir = Dir <$> (string "dir " *> many1 (satisfy isAlphaNum)) <*> pure []

parseListDir :: ReadP Cmd
parseListDir = ListDir <$> (string "$ ls" *> newline *> sepBy (parseFile +++ parseDir) newline)

parseCommands :: ReadP [Cmd]
parseCommands = sepBy (parseChangeDir +++ parseListDir) newline

nameOf :: FSItem -> String
nameOf (File name _) = name
nameOf (Dir name _) = name

cdUp :: FSZipper -> FSZipper
cdUp (item, FSContext name ls rs : bs) =
    (Dir name (ls ++ [item] ++ rs), bs)

cdRoot :: FSZipper -> FSZipper
cdRoot (item, []) = (item, [])
cdRoot fs = cdRoot $ cdUp fs

cdTo :: String -> FSZipper -> FSZipper
cdTo to (Dir parent items, bs) =
    let (ls, dir : rs) = break ((==) to . nameOf) items
     in (dir, FSContext parent ls rs : bs)

execCmd :: FSZipper -> Cmd -> FSZipper
execCmd fs (ChangeDir Root) = cdRoot fs
execCmd fs (ChangeDir Up) = cdUp fs
execCmd fs (ChangeDir (Down s)) = cdTo s fs
execCmd fs (ListDir items) = (Dir name items, bs)
  where
    (Dir name _, bs) = fs

isDir :: FSItem -> Bool
isDir (Dir _ _) = True
isDir _ = False

size :: FSItem -> Int
size (File _ size) = size
size (Dir _ items) = sum $ map size items

collect :: (FSItem -> Bool) -> FSItem -> [FSItem]
collect pred d@(Dir _ items) = [d | pred d] ++ concatMap (collect pred) items
collect pred file = [file | pred file]

part1 :: FSItem -> Int
part1 = sum . map size . collect smallEnough
  where
    smallEnough (File _ _) = False
    smallEnough (Dir _ items) = (sum $ map size items) <= 100000

part2 :: FSItem -> Int
part2 root = minimum . filter freesEnough . map size . collect isDir $ root
  where
    totalSpace = 70000000
    neededSpace = 30000000
    unusedSpace = totalSpace - size root
    freesEnough space = unusedSpace + space >= neededSpace

main :: IO ()
main = do
    cmds <- fst . last . readP_to_S parseCommands <$> readFile "input.txt"
    let root = fst . cdRoot . foldl' execCmd (Dir "/" [], []) $ cmds
    printf "Part 1: %d\n" $ part1 root
    printf "Part 2: %d\n" $ part2 root

-- Part 1:1447046
-- Part 2:578710
