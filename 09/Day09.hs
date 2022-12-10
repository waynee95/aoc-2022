import Prelude hiding (Left, Right, init)

import Control.Monad
import Control.Monad.State
import Data.Char (isDigit)
import Data.List (nub)
import Text.ParserCombinators.ReadP hiding (get)
import Text.Printf (printf)

import Data.Set as S

type Pos = (Int, Int)

data Direction = Left | Right | Up | Down
    deriving (Show)

type Rope = [Pos]
type History = S.Set Pos

newline :: ReadP Char
newline = char '\n'

parseInt :: ReadP Int
parseInt = read <$> munch1 isDigit

parseMove :: ReadP [Direction]
parseMove = do
    d <-
        choice
            [ Left <$ string "L "
            , Right <$ string "R "
            , Up <$ string "U "
            , Down <$ string "D "
            ]
    n <- parseInt
    return $ replicate n d

parseInput :: ReadP [Direction]
parseInput = concat <$> sepBy parseMove newline

move :: Direction -> Pos -> Pos
move Left (x, y) = (x - 1, y)
move Right (x, y) = (x + 1, y)
move Up (x, y) = (x, y + 1)
move Down (x, y) = (x, y - 1)

follow :: Pos -> Pos -> Pos
follow (hx, hy) tp@(tx, ty)
    | abs dx > 1 || abs dy > 1 = (tx + signum dx, ty + signum dy)
    | otherwise = tp
  where
    dx = hx - tx
    dy = hy - ty

execStep :: Direction -> State (Rope, History) Int
execStep d = do
    s <- get
    let (h : ts, ps) = s
        r' = scanl1 follow (move d h : ts)
        ps' = S.insert (last r') ps
    put $ (r', ps')
    return $ S.size ps'

execSteps :: [Direction] -> Int -> Int
execSteps ds n = last $ evalState (mapM execStep ds) $ init n

init :: Int -> (Rope, History)
init n = (rope n, S.empty)
  where
    rope n = replicate n (0, 0)

main :: IO ()
main = do
    ds <- fst . last . readP_to_S parseInput <$> readFile "input.txt"
    printf "Part 1: %d\n" $ execSteps ds 2
    printf "Part 2: %d\n" $ execSteps ds 10

-- Part 1: 6332
-- Part 2: 2511
