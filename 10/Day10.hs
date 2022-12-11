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

cycles :: [Int]
cycles = [20, 60 .. 220]

pixel :: Int -> Int -> Char
pixel r c
    | abs (r - c `mod` 40) < 2 = '#'
    | otherwise = '.'

execInstr :: Instr -> State (Int, Int, Int) Char
execInstr instr = case instr of
    Noop -> go 0
    Add n -> go n
  where
    go :: Int -> State (Int, Int, Int) Char
    go n = do
        (c, r, s) <- get
        let c' = succ c
            r' = r + n
            s' = s + if c' `elem` cycles then c' * r else 0
        put (c', r', s')
        return $ pixel r c

execInstrs :: [Instr] -> ([Char], (Int, Int, Int))
execInstrs instrs = runState (mapM execInstr instrs) init
  where
    init = (0, 1, 0)

part1 :: [Instr] -> Int
part1 = third . snd . execInstrs
  where
    third (a, b, c) = c

part2 :: [Instr] -> String
part2 = unlines . chunksOf 40 . fst . execInstrs

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
