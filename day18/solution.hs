import Data.Set qualified as S
import Data.List
import Debug.Trace
import Numeric

data Dir = East | South | West | North
  deriving (Eq, Show, Ord, Enum)

parseLine :: String -> (Dir, Int)
parseLine s = (dir d, read steps)
  where
    [d, steps, color] = words s
    dir "R" = East
    dir "L" = West
    dir "U" = North
    dir "D" = South

parseLine' :: String -> (Dir, Int)
parseLine' s = (dir, steps)
  where
    [_, _, color] = words s
    steps = fst $ head $ readHex $ take 5 $ drop 2 color
    dir = [East, South, West, North] !! (fst $ head $ readHex [last $ take 6 $ drop 2 color])


executePlan :: [(Dir, Int)] -> [(Int, Int)] -> [(Int,Int)]
executePlan [] vertices = reverse vertices
executePlan ((dir,steps):rest) vertices = executePlan rest  (pos':vertices)
  where
    pos = head vertices
    pos'= walk dir steps pos

walk :: Dir -> Int -> (Int, Int) -> (Int, Int)
walk North n (r,c) = (r-n, c)
walk South n (r,c) = (r+n, c)
walk West n (r,c) = (r, c-n)
walk East n (r,c) = (r, c+n)

shoeLace :: [(Int,Int)] -> Int
shoeLace vs = abs $ sum (term <$> [0..n-2]) `div` 2
  -- The sum runs to n-2 because the first vertex is included at
  -- the beginning and end of the list.
  where
    term i = (xs !! (i+1) + xs !! i) * (ys !! (i+1) - ys !! i)
    n = length vs
    xs = fst <$> vs
    ys = snd <$> vs

-- Pick's thereom gives A = i + b/2 - 1 for the area of a polygon with
-- vertices on lattice points where i is the number of interior
-- lattice points and b is the number of lattice points on the
-- boundary. If we take the cells of the map to be centered on lattice
-- points then the area we are interested in is equal to the number of
-- boundary lattice points (the trench) plus the interior lattice
-- points (the remaining excavation). That is A' = (b + i). We can use
-- the shoelace thereom to get the area A of the polygon.
--
-- A = i + b/2 -1
-- A + 1 = i + b/2
-- A + 1 + b/2 = i + b
-- A' = A + b/2 + 1

solve steps = shoeLace vertices + b `div` 2 + 1
  where
    vertices = executePlan steps [(0,0)]
    b = sum $ snd <$> steps

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  putStrLn "Part 1:"
  print $ solve $ parseLine <$> input
  putStrLn "Part 2:"
  print $ solve $ parseLine' <$> input
