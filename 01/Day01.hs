import Data.Char (isDigit)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Text.ParserCombinators.ReadP
import Text.Printf (printf)

newline :: ReadP Char
newline = char '\n'

emptyLine :: ReadP String
emptyLine = string "\n\n"

parseInt :: ReadP Int
parseInt = read <$> munch1 isDigit

parseIntBlock :: ReadP [Int]
parseIntBlock = sepBy parseInt newline

parseInput :: ReadP [[Int]]
parseInput = sepBy parseIntBlock emptyLine

part1 :: [[Int]] -> Int
part1 = maximum . map sum

part2 :: [[Int]] -> Int
part2 = sum . take 3 . sortOn Down . map sum

main :: IO ()
main = do
    xs <- fst . last . readP_to_S parseInput <$> readFile "input.txt"
    printf "Part 1: %d\n" $ part1 xs
    printf "Part 2: %d\n" $ part2 xs

-- Part 1: 68467
-- Part 2: 203420
