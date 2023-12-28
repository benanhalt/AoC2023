import Data.Char
import Data.List

calibrationValue :: String -> Int
calibrationValue s = read (head digits : [last digits])
  where digits = filter isDigit s

spelledOutDigit = words "zero one two three four five six seven eight nine"

insertDigits "" = ""
insertDigits s@(c:cs) = case findIndex (`isPrefixOf` s) spelledOutDigit of
  Just i -> intToDigit i:insertDigits cs
  Nothing -> c:insertDigits cs

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  putStrLn "Part 1:"
  print $ sum $ calibrationValue <$> input
  putStrLn "Part 2:"
  print $ sum $ calibrationValue . insertDigits <$> input

