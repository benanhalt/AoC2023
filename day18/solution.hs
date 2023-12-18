import Data.Set qualified as S
import Data.List
import Debug.Trace
import Numeric

data Dir = East | South | West | North
  deriving (Eq, Show, Ord, Enum)

parseLine :: String -> (Dir, Int, String)
parseLine s = (dir d, read steps, color)
  where
    [d, steps, color] = words s
    dir "R" = East
    dir "L" = West
    dir "U" = North
    dir "D" = South

parseLine' :: String -> (Dir, Int, String)
parseLine' s = (dir, steps, "")
  where
    [_, _, color] = words s
    steps = fst $ head $ readHex $ take 5 $ drop 2 color
    dir = [East, South, West, North] !! (fst $ head $ readHex [last $ take 6 $ drop 2 color])


executePlan :: [(Dir, Int, String)] -> [(Int, Int)] -> [(Int,Int)]
executePlan [] vertices = reverse vertices
executePlan ((dir,steps,_):rest) vertices = executePlan rest  (pos':vertices)
  where
    pos = head vertices
    pos'= walk dir steps pos

computeCorners :: [Dir] -> [Int]
computeCorners dirs = compute 3 1 turns
  where
    enumed = fromEnum <$> dirs
    turns = (`mod` 4) . (+ 4) <$> zipWith (-) enumed (last enumed : enumed)
    compute lastCorner lastTurn [] = []
    compute lastCorner lastTurn (turn:turns) = (corner : compute corner turn turns)
      where corner =
              if turn /= lastTurn
              then lastCorner
              else (lastCorner + turn) `mod` 4

walk :: Dir -> Int -> (Int, Int) -> (Int, Int)
walk North n (r,c) = (r-n, c)
walk South n (r,c) = (r+n, c)
walk West n (r,c) = (r, c-n)
walk East n (r,c) = (r, c+n)

shoeString :: [(Int,Int)] -> Int
shoeString vs = sum (term <$> [0..n-2]) `div` 2
  where
    term i = (xs !! (i+1) + xs !! i) * (ys !! (i+1) - ys !! i)
    n = length vs
    xs = fst <$> vs
    ys = snd <$> vs


adjustVertex :: (Int, Int) -> Int -> (Int, Int)
adjustVertex (r,c) 0 = (r,c)
adjustVertex (r,c) 1 = (r,c+1)
adjustVertex (r,c) 2 = (r+1,c+1)
adjustVertex (r,c) 3 = (r+1,c)


solve steps =
  let corners = computeCorners $ (\(dir,_,_) -> dir) <$> steps
      vertices = zipWith adjustVertex (executePlan steps [(0,0)]) (cycle corners)
  in abs $ shoeString vertices

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  putStrLn "Part 1:"
  print $ solve $ parseLine <$> input
  putStrLn "Part 2:"
  print $ solve $ parseLine' <$> input
