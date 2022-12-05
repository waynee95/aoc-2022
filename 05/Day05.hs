{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad
import Data.Char (isAsciiUpper, isDigit)
import Data.List
import Data.Maybe (catMaybes)
import Text.ParserCombinators.ReadP
import Text.Printf (printf)

type Crate = Char
type Stack = [Crate]

data Instruction = Instruction {n :: Int, from :: Int, to :: Int}
    deriving (Show)

newline :: ReadP Char
newline = char '\n'

parseCrate :: ReadP Crate
parseCrate = do
    void $ char '['
    c <- satisfy isAsciiUpper
    void $ char ']'
    return $ c

parseMaybeCrate :: ReadP (Maybe Crate)
parseMaybeCrate = do
    choice
        [ Just <$> parseCrate
        , Nothing <$ count 3 (char ' ')
        ]

parseCrateRow :: ReadP [Maybe Crate]
parseCrateRow = sepBy parseMaybeCrate (char ' ')

parseInt :: ReadP Int
parseInt = read <$> munch1 isDigit

parseInstruction :: ReadP Instruction
parseInstruction = do
    void $ string "move "
    n <- parseInt
    void $ string " from "
    from <- pred <$> parseInt
    void $ string " to "
    to <- pred <$> parseInt
    return Instruction{n, from, to}

parseStacksAndInstructions :: ReadP ([Stack], [Instruction])
parseStacksAndInstructions = do
    crateRows <- sepBy parseCrateRow newline
    void $ many (choice [char ' ', satisfy isDigit]) <* newline
    void newline
    instructions <- sepBy parseInstruction newline
    let stacks = map catMaybes $ transpose crateRows
    return (stacks, instructions)

-- replace list item at index i with x
replace :: Int -> a -> [a] -> [a]
replace i x xs = take i xs ++ x : drop (i + 1) xs

moveN :: Int -> Stack -> Stack -> (Stack, Stack)
moveN n from to = (new, remaining)
  where
    new = take n from ++ to
    remaining = drop n from

execMove :: Int -> [Stack] -> Int -> Int -> [Stack]
execMove n stacks from to = stacks'
  where
    fromst = stacks !! from
    tost = stacks !! to
    (new, remaining) = moveN n fromst tost
    stacks' = replace to new (replace from remaining stacks)

tops = map head . filter (not . null)

-- not the most elegant but it works ¯\_(ツ)_/¯
part1 st [] = tops st
part1 st (Instruction{n = 0, from, to} : rest) = part1 st rest
part1 st (Instruction{n, from, to} : rest) =
    part1 (execMove 1 st from to) (Instruction{n = pred n, from, to} : rest)

part2 st [] = tops st
part2 st (Instruction{n, from, to} : rest) =
    part2 (execMove n st from to) rest

main :: IO ()
main = do
    (st, intrs) <- fst . last . readP_to_S parseStacksAndInstructions <$> readFile "input.txt"
    printf "Part 1: %s\n" $ part1 st intrs
    printf "Part 2: %s\n" $ part2 st intrs

-- Part 1: RFFFWBPNS
-- Part 2: CQQBBJFCS
