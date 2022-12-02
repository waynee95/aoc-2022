module Day02 where

import Text.Printf

data Move = Rock | Paper | Scissors deriving (Show, Eq)
data Outcome = Lost | Draw | Won deriving (Show)

parseInput enemy player input = map (\[x, y] -> (enemy x, player y)) . map words . lines $ input

enemy "A" = Rock
enemy "B" = Paper
enemy "C" = Scissors

response "X" = Rock
response "Y" = Paper
response "Z" = Scissors

outcome "X" = Lost
outcome "Y" = Draw
outcome "Z" = Won

scoreMove Rock = 1
scoreMove Paper = 2
scoreMove Scissors = 3

scoreOutcome Lost = 0
scoreOutcome Draw = 3
scoreOutcome Won = 6

scoring (result, selectedMove) = scoreOutcome result + scoreMove selectedMove

beatby Rock = Paper
beatby Scissors = Rock
beatby Paper = Scissors

beats Rock = Scissors
beats Scissors = Paper
beats Paper = Rock

selectMove move Draw = move
selectMove move Won = beatby move
selectMove move Lost = beats move

part1 :: [(Move, Move)] -> Int
part1 = sum . map scoring . map result
  where
    result (x, y) | x == y = (Draw, y)
    result (x, y) | x == beats y = (Won, y)
    result (x, y) = (Lost, y)

part2 :: [(Move, Outcome)] -> Int
part2 = sum . map scoring . map result
  where
    result (x, y) = (y, selectMove x y)

main :: IO ()
main = do
    input <- readFile "input.txt"
    printf "Part 1: %d\n" $ part1 (parseInput enemy response input)
    printf "Part 2: %d\n" $ part2 (parseInput enemy outcome input)
