{-# LANGUAGE OverloadedStrings #-}

module Day01 where

import Data.List
import Text.Printf

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

parseInput :: T.Text -> [[Int]]
parseInput input = intList
  where
    split = T.splitOn "\n\n" input
    strList = map T.lines split
    intList = map (\list -> map (\(Right (n, _)) -> n) (map T.decimal list)) strList

part1 :: [[Int]] -> Int
part1 = maximum . map sum

part2 :: [[Int]] -> Int
part2 = sum . take 3 . sortBy (flip compare) . map sum

main :: IO ()
main = do
    intList <- parseInput <$> T.readFile "input.txt"
    printf "Part 1: %d\n" $ part1 intList
    printf "Part 2: %d\n" $ part2 intList
