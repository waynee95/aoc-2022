import Data.Char (digitToInt)
import Data.List (transpose, zip4, zipWith)
import Text.Printf (printf)

parseInput :: String -> [[Int]]
parseInput = map (map digitToInt) . lines

part1 :: [[Int]] -> Int
part1 grid = length . filter id . map or4 $ zip4 (concat left) (concat right) (concat top) (concat bottom)
  where
    left = map visible grid
    right = map visible' grid
    top = transpose . map visible . transpose $ grid
    bottom = transpose . map visible' . transpose $ grid
    visible' = reverse . visible . reverse
    or4 (a, b, c, d) = or [a, b, c, d]

part2 :: [[Int]] -> Int
part2 grid = maximum $ zipWith (*) horz vert
  where
    left = map score grid
    right = map score' grid
    top = transpose . map score $ transpose grid
    bottom = transpose . map score' $ transpose grid
    score' = reverse . score . reverse
    horz = zipWith (*) (concat left) (concat right)
    vert = zipWith (*) (concat top) (concat bottom)

-- visible from left
visible :: [Int] -> [Bool]
visible = go (-1)
  where
    go _ [] = []
    go _ [x] = [True]
    go h (x : xs)
        | x > h = True : go x xs
        | otherwise = False : go h xs

score :: [Int] -> [Int]
score [x] = [0]
score (x : xs) = min (length xs) (1 + length (takeWhile (< x) xs)) : score xs

main :: IO ()
main = do
    grid <- parseInput <$> readFile "input.txt"
    printf "Part 1: %d\n" $ part1 grid
    printf "Part 2: %d\n" $ part2 grid

-- Part 1: 1818
-- Part 2: 368368
