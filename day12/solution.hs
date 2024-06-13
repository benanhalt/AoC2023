import Data.List (intercalate)
import Data.List.Split (wordsBy, splitOn)
import Debug.Trace

type Row = (String, [Int])

parseRow :: String -> Row
parseRow s =
  let
    [image, counts] = words s
  in
    (image, read <$> splitOn "," counts)

posCounts :: String -> [Maybe Int]
posCounts s =
  let
    groups = wordsBy (== '.') s
    countGroup g = if '?' `elem` g then Nothing else Just $ length g
  in
    countGroup <$> groups

doCounts :: String -> [Int]
doCounts s = length <$> wordsBy (== '.') s

consistent :: [Maybe Int] -> [Int] -> Bool
consistent [] [] = True
consistent (Just p:ps) (q:qs) = p == q && consistent ps qs
consistent _ _ = True

enumerate :: String -> [String]
enumerate ""  = [""]
enumerate ('.':rest) = ["." ++ x | x <- enumerate rest]
enumerate ('#':rest) = ["#" ++ x | x <- enumerate rest]
enumerate ('?':rest) = ["." ++ x | x <- enumerate rest] ++ ["#" ++ x | x <- enumerate rest]

-- enumerate :: String -> [String]
-- enumerate s = enumerate' "" s (const True)

possibilities :: Row -> Int
possibilities (image, counts) = traceShowId $ length $ filter (== counts) $ doCounts <$> enumerate image 
--possibilities (image, counts) = traceShowId $ length $ enumerate' "" image (\i -> consistent (posCounts i) counts)

enumerate' :: String -> String -> (String -> Bool) -> [String]
enumerate' pre [] _ = [pre]
enumerate' pre ('?':rest) ok =
  (if ok (pre ++ "." ++ rest) then enumerate' (pre ++ ".") rest ok else []) ++
  (if ok (pre ++ "#" ++ rest) then enumerate' (pre ++ "#") rest ok else [])
enumerate' pre (c:rest) ok = enumerate' (pre ++ [c]) rest ok

unfoldRow :: Row -> Row
unfoldRow (image, counts) = (intercalate "?" $ replicate 5 image, concat $ replicate 5 counts)

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  print $ sum $ possibilities <$> parseRow <$> input

