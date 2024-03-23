import Data.List.Split (splitOn)
import Data.List (find, foldl', sort)
import Data.Maybe (maybe)


parseRange :: String -> (Int, Int, Int)
parseRange s =
  let
    [dest, src, len] = read <$> words s
  in
    (dest, src, len)

applyMap :: [(Int, Int, Int)] -> Int -> Int
applyMap m n = maybe n (\(dest, src, _) -> dest + n - src) $
  find (\(_, src, len) -> src <= n && n <= src + len) m

applyMaps :: [[(Int, Int, Int)]] -> Int -> Int
applyMaps ms n = foldl' (flip applyMap) n ms

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let seeds :: [Int] = read <$> (words $ filter (`elem` "1234567890 ") $ head input)
  let maps = (map parseRange) <$> drop 1 <$> (splitOn [""] $ drop 2 input)
  let results = applyMaps maps <$> seeds
  print $ sort results
  
