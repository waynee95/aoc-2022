{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (void)
import Control.Monad.State
import Data.Char (isDigit)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Text.ParserCombinators.ReadP hiding (get)
import Text.Printf (printf)

import qualified Data.IntMap as M
import qualified Data.Sequence as Seq

data OpType = Add | Mult
    deriving (Show, Eq)

data Operand = Old | Num Int
    deriving (Show, Eq)

data Operation = Operation OpType Operand Operand
    deriving (Show, Eq)

-- Test d id1 id2 = if x mod d then id1 else id2
data Test = Test Int Int Int
    deriving (Show, Eq)

data Monkey = Monkey
    { _id :: Int
    , _items :: Seq.Seq Int
    , _op :: Operation
    , _test :: Test
    , _inspections :: Int
    }
    deriving (Show, Eq)

type Monkeys = M.IntMap Monkey

newline :: ReadP Char
newline = char '\n'

emptyline :: ReadP String
emptyline = count 2 newline

indent :: ReadP String
indent = count 2 (char ' ')

parseInt :: ReadP Int
parseInt = read <$> munch1 isDigit

parseIntList :: ReadP [Int]
parseIntList = sepBy parseInt (string ", ")

parseStartingItems :: ReadP [Int]
parseStartingItems = do
    void $ indent
    string "Starting items: "
    xs <- parseIntList
    void $ newline
    return xs

parseOpType :: ReadP OpType
parseOpType =
    choice
        [ Add <$ string "+"
        , Mult <$ string "*"
        ]

parseOperand :: ReadP Operand
parseOperand =
    choice
        [ Old <$ (skipSpaces *> string "old" <* skipSpaces)
        , Num <$> (skipSpaces *> parseInt <* skipSpaces)
        ]

parseOperation :: ReadP Operation
parseOperation = do
    void $ indent *> string "Operation: new ="
    left <- parseOperand
    op <- parseOpType
    right <- parseOperand
    return $ Operation op left right

parseTest :: ReadP Test
parseTest = do
    string "Test: divisible by "
    x <- parseInt
    void $ newline *> indent *> indent
    string "If true: throw to monkey "
    id1 <- parseInt
    void $ newline *> indent *> indent
    string "If false: throw to monkey "
    id2 <- parseInt
    return $ Test x id1 id2

parseMonkey :: ReadP Monkey
parseMonkey = do
    string "Monkey "
    _id <- parseInt
    void $ char ':' *> newline
    _items <- Seq.fromList <$> parseStartingItems
    _op <- parseOperation
    _test <- parseTest
    return $ Monkey {_inspections = 0, ..}

parseInput :: ReadP [Monkey]
parseInput = sepBy parseMonkey emptyline

doOperation :: Operation -> Int -> Int
doOperation (Operation op a b) level = case op of
    Add -> (value a level) + (value b level)
    Mult -> (value a level) * (value b level)
  where
    value Old old = old
    value (Num n) old = n

doTest :: Test -> Int -> Int
doTest (Test d id1 id2) n =
    if n `mod` d == 0 then id1 else id2

catchItem :: Item -> Int -> State Monkeys ()
catchItem item key = update key $
    \m -> m {_items = _items m Seq.|> item}

updateMonkey :: Int -> Int -> State Monkeys ()
updateMonkey key n = update key $
    \m -> m {_items = Seq.empty, _inspections = _inspections m + n}

update :: Int -> (Monkey -> Monkey) -> State Monkeys ()
update key = modify . flip M.adjust key

doRound :: (Int -> Int) -> State Monkeys ()
doRound relief = do
    keys <- gets M.keys
    forM_ keys $ \key -> do
        Monkey {..} <- gets (M.! key)
        let len = length _items
        forM_ _items $ \item -> do
            let worry = relief $ doOperation _op item
                target = doTest _test worry
            catchItem worry target
        updateMonkey key len

doRounds :: (Int -> Int) -> Int -> Monkeys -> Monkeys
doRounds relief n = execState (replicateM_ n $ doRound relief)

solve :: (Int -> Int) -> Int -> Monkeys -> Int
solve relief n mp = product . take 2 . sortOn Down . map _inspections . M.elems $ doRounds relief n mp

part1 :: Monkeys -> Int
part1 = solve (`div` 3) 20

part2 :: Monkeys -> Int
part2 mp = solve (`mod` divisor) 10_000 mp
  where
    divisor = product . map f . M.elems $ mp
    f Monkey {_test = Test d _ _} = d

main :: IO ()
main = do
    ms <- fst . last . readP_to_S parseInput <$> readFile "input.txt"
    let mp = M.fromList (zip [0 ..] ms)
    printf "Part 1: %d\n" $ part1 mp
    printf "Part 2: %d\n" $ part2 mp

-- Part 1: 78960
-- Part 2: 14561971968
