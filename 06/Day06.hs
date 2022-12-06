import Data.Char (isSpace)
import Data.List (dropWhileEnd, nub)
import Text.Printf (printf)

trim = dropWhileEnd isSpace . dropWhileEnd isSpace

unique xs = xs == nub xs

findMarker :: Int -> Int -> String -> Int
findMarker l n s | unique (take l s) = n + l
findMarker l n s = findMarker l (succ n) (tail s)

main :: IO ()
main = do
    s <- trim <$> readFile "input.txt"
    printf "Part 1: %d\n" $ findMarker 4 0 s
    printf "Part 2: %d\n" $ findMarker 14 0 s

-- Part 1: 1544
-- Part 2: 2145
