import Control.Monad.State
import Data.Char (isDigit)
import Data.List (find)
import Text.ParserCombinators.ReadP hiding (get)
import Text.Printf (printf)

data Instr = Add Int | Noop
    deriving (Show)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = ls : chunksOf n rs
  where
    (ls, rs) = splitAt n xs

newline :: ReadP Char
newline = char '\n'

parseSignedInt :: ReadP Int
parseSignedInt = read <$> ((:) <$> parseSign <*> parseDigits)
  where
    parseSign = option ' ' (char '-')
    parseDigits = munch1 isDigit

parseInstr :: ReadP Instr
parseInstr = parseAdd +++ parseNoop
  where
    parseAdd = Add <$> (string "addx " *> parseSignedInt)
    parseNoop = Noop <$ string "noop"

parseInput :: ReadP [Instr]
parseInput =
    concat
        <$> many
            ( do
                op <- parseInstr
                newline
                case op of
                    Noop -> return [Noop]
                    Add x -> return [Noop, Add x]
            )

execInstr :: Instr -> State Int Int
execInstr Noop = get
execInstr (Add n) = do
    r <- get
    put $ r + n
    return r

execInstrs :: [Instr] -> [Int]
execInstrs instrs = evalState (mapM execInstr instrs) init
  where
    init = 1

strength :: [Int] -> Int -> Int
strength xs i = i * (xs !! i)

part1 :: [Instr] -> Int
part1 instrs = sum . map (strength (0 : execInstrs instrs)) $ [20, 60 .. 220]

pixel :: Int -> Int -> Char
pixel r c
    | abs (r - c `mod` 40) < 2 = '#'
    | otherwise = '.'

part2 :: [Instr] -> String
part2 instrs = unlines . chunksOf 40 $ zipWith pixel (execInstrs instrs) [0 ..]

main :: IO ()
main = do
    instrs <- fst . last . readP_to_S parseInput <$> readFile "input.txt"
    printf "Part 1: %d\n" $ part1 instrs
    printf "Part 2:\n%s" $ part2 instrs

-- Part 1: 14820
-- Part 2:
-- ###..####.####.#..#.####.####.#..#..##..
-- #..#....#.#....#.#..#....#....#..#.#..#.
-- #..#...#..###..##...###..###..####.#..#.
-- ###...#...#....#.#..#....#....#..#.####.
-- #.#..#....#....#.#..#....#....#..#.#..#.
-- #..#.####.####.#..#.####.#....#..#.#..#.
