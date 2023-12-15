import Data.List
import Data.Map.Strict qualified as M


roll :: String -> Int -> Int -> String -> String
roll ('.':rest) n m r = roll rest (n+1) m r
roll ('#':rest) n m r = roll rest (n+1) (n+1) (r ++ replicate (n-m) '.' ++ "#")
roll ('O':rest) n m r = roll rest (n+1) (m+1) (r ++ "O")
roll [] n m r = r ++ replicate (n-m) '.'

rollNorth :: [String] -> [String]
rollNorth grid = transpose $ map (\s -> roll s 0 0 "") $ transpose grid

score :: [String] -> Int
score rows = sum $ score' <$> transpose rows

score' :: String -> Int
score' col = sum $ map snd $ filter ((== 'O') . fst) $ zip col $ reverse [0..length col]

-- Rotate a layout 90 degrees.
rotate = transpose . reverse

spinCycle
  = rotate . rollNorth
  . rotate . rollNorth
  . rotate . rollNorth
  . rotate . rollNorth

-- Find first repeated grid layout. Return the number of steps at
-- which the repeats occur and the repeatd layout.
findRepeat m n grid = case M.lookup grid m of
  Nothing -> findRepeat (M.insert grid n m) (n + 1) (spinCycle grid)
  Just n' -> (n', n, grid)

main :: IO ()
main = do
  rows <- lines <$> readFile "input.txt"
  putStrLn "Part 1:"
  print $ score $ rollNorth rows
  putStrLn "Part 2:"
  let end = 1000000000
  let (n0, n1, repeated) = findRepeat M.empty 0 rows
  let l = n1 - n0 -- Cycle length
  let k = (end - n0) `div` l -- Number of repeats before end
  let p = end - (k * l + n0) -- Number of steps to get to end after k repeats
  let result = iterate spinCycle repeated !! p -- Skip all k repeats and only do end steps
  print $ score result
