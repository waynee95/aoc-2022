{-# LANGUAGE OverloadedStrings #-}

module Day04 where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

import Text.Printf

parseInput :: T.Text -> [((Int, Int), (Int, Int))]
parseInput = map rangeToNums . map splitRanges . map (T.splitOn ",") . T.lines
  where
    splitRange r = T.splitOn "-" r
    splitRanges [x, y] = (splitRange x, splitRange y)

    toNum s = let (Right (n, _)) = T.decimal s in n
    rangeToNums ([x1, x2], [y1, y2]) = ((toNum x1, toNum x2), (toNum y1, toNum y2))

overlapFully :: (Int, Int) -> (Int, Int) -> Bool
overlapFully (x1, x2) (y1, y2) = (x1 >= y1 && x2 <= y2) || (y1 >= x1 && y2 <= x2)

overlapAny :: (Int, Int) -> (Int, Int) -> Bool
overlapAny (x1, x2) (y1, y2) =
    (x1 <= y1 && y1 <= x2)
        || (x1 <= y2 && y2 <= x2)
        || (y1 <= x1 && x1 <= y2)
        || (y1 <= x2 && x2 <= y2)

solve f = length . filter (uncurry f)

main :: IO ()
main = do
    rangeList <- parseInput <$> T.readFile "input.txt"
    printf "Part 1: %d\n" $ solve overlapFully rangeList
    printf "Part 2: %d\n" $ solve overlapAny rangeList
