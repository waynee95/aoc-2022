import Control.Monad
import Data.Char (isAlphaNum, isDigit, isLetter, isPunctuation)
import Data.List (foldl')
import Text.ParserCombinators.ReadP
import Text.Printf (printf)

data FSItem = File String Int | Folder String [FSItem]
    deriving (Show)

data Location = Root | Up | Down String
    deriving (Show)
data Cmd = ChangeDir Location | ListDir [FSItem]
    deriving (Show)

-- http://learnyouahaskell.com/zippers
-- parent folder, items before, items after
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
parseChangeDir = do
    void $ string "$ cd "
    loc <- parseLocation
    return $ ChangeDir loc

isFileName = liftM2 (||) isAlphaNum isPunctuation

parseFile :: ReadP FSItem
parseFile = do
    size <- parseInt
    void $ char ' '
    name <- many1 (satisfy isFileName)
    return $ File name size

parseDir :: ReadP FSItem
parseDir = do
    void $ string "dir "
    name <- many1 (satisfy isAlphaNum)
    return $ Folder name [] -- to be filled later

parseListDir :: ReadP Cmd
parseListDir = do
    void $ string "$ ls\n"
    content <- sepBy (parseFile +++ parseDir) newline
    return $ ListDir content

parseCommands :: ReadP [Cmd]
parseCommands = sepBy (parseChangeDir +++ parseListDir) newline

nameOf :: FSItem -> String
nameOf (File name _) = name
nameOf (Folder name _) = name

cdUp :: FSZipper -> FSZipper
cdUp (item, FSContext name ls rs : bs) =
    (Folder name (ls ++ [item] ++ rs), bs)

cdRoot :: FSZipper -> FSZipper
cdRoot (item, []) = (item, [])
cdRoot fs = cdRoot $ cdUp fs

cdTo :: String -> FSZipper -> FSZipper
cdTo to (Folder parent items, bs) =
    let (ls, dir : rs) = break ((==) to . nameOf) items
     in (dir, FSContext parent ls rs : bs)

execCmd :: FSZipper -> Cmd -> FSZipper
execCmd fs (ChangeDir Root) = cdRoot fs
execCmd fs (ChangeDir Up) = cdUp fs
execCmd fs (ChangeDir (Down s)) = cdTo s fs
execCmd fs (ListDir items) = (Folder name items, bs)
  where
    (Folder name _, bs) = fs

isFolder :: FSItem -> Bool
isFolder (Folder _ _) = True
isFolder _ = False

size :: FSItem -> Int
size (File _ size) = size
size (Folder _ items) = sum $ map size items

collect :: (FSItem -> Bool) -> FSItem -> [FSItem]
collect pred f@(Folder _ items) = [f | pred f] ++ concatMap (collect pred) items
collect pred file = [file | pred file]

part1 :: FSItem -> Int
part1 = sum . map size . collect smallEnough
  where
    smallEnough (File _ _) = False
    smallEnough (Folder _ items) = (sum $ map size items) <= 100000

part2 :: FSItem -> Int
part2 root = minimum . filter ((>= neededSpace) . (+ unusedSpace)) . map size . collect isFolder $ root
  where
    totalSpace = 70000000
    neededSpace = 30000000
    unusedSpace = totalSpace - size root

main :: IO ()
main = do
    cmds <- fst . last . readP_to_S parseCommands <$> readFile "input.txt"
    let root = fst . cdRoot . foldl' execCmd (Folder "/" [], []) $ cmds
    printf "Part 1:%d\n" $ part1 root
    printf "Part 2:%d\n" $ part2 root

-- Part 1:1447046
-- Part 2:578710
