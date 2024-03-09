import Data.List
import Data.Foldable (toList)
import Data.Map.Strict qualified as M
import Control.Monad

type Grid = M.Map (Int,Int) Char

neighbors :: Char -> (Int, Int) -> [(Int, Int)]
neighbors '|' (r,c) = [(r-1,c), (r+1,c)]
neighbors '-' (r,c) = [(r,c-1), (r,c+1)]
neighbors 'L' (r,c) = [(r-1,c), (r,c+1)]
neighbors 'J' (r,c) = [(r-1,c), (r,c-1)]
neighbors '7' (r,c) = [(r+1,c), (r,c-1)]
neighbors 'F' (r,c) = [(r+1,c), (r,c+1)]

start :: Grid -> ((Int, Int), (Int, Int))
start grid =
  let
    Just ((r,c), char) = find (\(s, char) -> char == 'S') $ M.assocs grid
    s1 = head $ do
      s <- [(r+1,c), (r-1,c), (r,c+1), (r,c-1)]
      char' <- toList $ M.lookup s grid
      n <- neighbors char' s
      guard $ n == (r,c)
      pure s
  in ((r,c), s1)

step :: (Int, Int) -> (Int, Int) -> Char -> (Int, Int)
step (r',c') (r,c) '|' | r == r'+1 = (r+1, c)
step (r',c') (r,c) '|' | r == r'-1 = (r-1, c)
step (r',c') (r,c) '-' | c == c'+1 = (r, c+1)
step (r',c') (r,c) '-' | c == c'-1 = (r, c-1)
step (r',c') (r,c) 'L' | c == c'-1 = (r-1, c)
step (r',c') (r,c) 'L' | r == r'+1 = (r, c+1)
step (r',c') (r,c) 'J' | c == c'+1 = (r-1, c)
step (r',c') (r,c) 'J' | r == r'+1 = (r, c-1)
step (r',c') (r,c) '7' | c == c'+1 = (r+1, c)
step (r',c') (r,c) '7' | r == r'-1 = (r, c-1)
step (r',c') (r,c) 'F' | c == c'-1 = (r+1, c)
step (r',c') (r,c) 'F' | r == r'-1 = (r, c+1)
step prev curr char = error $ show (prev,curr,char)

move :: Grid -> (Int, Int) -> (Int, Int) -> (Int, Int)
move grid prev curr = step prev curr (grid M.! curr)

update :: Grid -> ((Int, Int), (Int, Int)) -> ((Int, Int), (Int,Int))
update grid (prev, curr) = (curr, move grid prev curr)


shoeLace :: [(Int,Int)] -> Int
shoeLace vs = abs $ sum (term <$> [0..n-1]) `div` 2
  where
    term i = (xs !! (i+1) + xs !! i) * (ys !! (i+1) - ys !! i)
    n = length vs
    xs = fst <$> cycle vs
    ys = snd <$> cycle vs


main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let grid = M.fromList $ do
        (r, l) <- zip [1..] input
        (c, char) <- zip [0..] l
        pure ((r,c), char)
  let (s0, s1) = start grid
  let loop = takeWhile ((/= s0) . snd) $ iterate (update grid) (s0, s1)
  let dist = (1 + length loop) `div` 2
  putStrLn "Part 1:"
  print dist
  let nodes :: [(Int, Int)] = snd <$> loop
  putStrLn "Part 2:"
  print $ shoeLace nodes - dist + 1
