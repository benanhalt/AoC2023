import Data.List
import Data.List.Split
import Data.Map.Strict qualified as Map
import Debug.Trace

countMatches :: String -> Int
countMatches line = length $ intersect winning mine
  where
    [_, numbers] = splitOn ": " line
    [winning, mine] = words <$> splitOn " | " numbers

scoreCard' :: [Int] -> Int -> Int
scoreCard' matches cardN = 1 + sum (scoreCard' matches <$> others)
  where
    thisScore = matches !! cardN
    others = [cardN + 1 .. cardN + thisScore]

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  putStrLn "Part 1:"
  let matches = countMatches <$> input
  let scores = (\matching -> if matching > 0 then 2 ^ (matching - 1) else 0) <$> matches
  print $ sum scores
  putStrLn "Part 2:"
  print $ sum $ scoreCard' matches <$> [0..length scores - 1]
