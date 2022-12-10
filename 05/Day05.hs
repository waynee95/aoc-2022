import Control.Monad
import Data.Char (isAsciiUpper, isDigit)
import Data.List
import Data.Maybe (catMaybes)
import Text.ParserCombinators.ReadP
import Text.Printf (printf)

type Crate = Char
type Stack = [Crate]
type Instruction = (Int, Int, Int)

newline :: ReadP Char
newline = char '\n'

parseCrate :: ReadP Crate
parseCrate = do
    void $ char '['
    c <- satisfy isAsciiUpper
    void $ char ']'
    return c

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
    return (n, from, to)

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

moveN :: (Stack -> Stack) -> Int -> Stack -> Stack -> (Stack, Stack)
moveN modifier n from to = (new, remaining)
  where
    new = modifier (take n from) ++ to
    remaining = drop n from

execInstr :: (Stack -> Stack) -> [Stack] -> Instruction -> [Stack]
execInstr modifier st (n, from, to) = st'
  where
    fromst = st !! from
    tost = st !! to
    (new, remaining) = moveN modifier n fromst tost
    st' = replace to new (replace from remaining st)

solve :: (Stack -> Stack) -> [Stack] -> [Instruction] -> String
solve modifier st instrs = tops $ foldl (execInstr modifier) st instrs
  where
    -- collect top element of resulting stacks
    tops = map head . filter (not . null)

main :: IO ()
main = do
    (st, instrs) <- fst . last . readP_to_S parseStacksAndInstructions <$> readFile "input.txt"
    printf "Part 1: %s\n" $ solve reverse st instrs
    printf "Part 2: %s\n" $ solve id st instrs

-- Part 1: RFFFWBPNS
-- Part 2: CQQBBJFCS
