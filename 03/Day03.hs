module Day03 where

import Data.Char
import Data.List
import Text.Printf

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = as : chunksOf n bs
  where
    (as, bs) = splitAt n xs

split s = splitAt ((length s) `div` 2) s

priority [c] | isLower c = let (Just i) = elemIndex c ['a' .. 'z'] in i + 1
priority [c] = let (Just i) = elemIndex c ['A' .. 'Z'] in i + 27

part1 :: [(String, String)] -> Int
part1 = sum . map priority . map result
  where
    result (t1, t2) = foldr f [] t1
      where
        f c acc = if elem c t2 && not (elem c acc) then c : acc else acc

part2 :: [[String]] -> Int
part2 = sum . map priority . map result
  where
    result [x, y, z] = foldr f [] x
      where
        f c acc = if elem c y && elem c z && not (elem c acc) then c : acc else acc

main :: IO ()
main = do
    f <- readFile "input.txt"
    printf "Part 1: %d\n" $ part1 (map split (lines f))
    printf "Part 2: %d\n" $ part2 (chunksOf 3 . lines $ f)
