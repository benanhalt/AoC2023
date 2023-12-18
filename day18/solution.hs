import Data.Set qualified as S
import Data.List
import Debug.Trace
import Numeric

parseLine :: String -> (String, Int, String)
parseLine s = (dir, read steps, color)
  where
    [dir, steps, color] = words s

parseLine' :: String -> (String, Int, String)
parseLine' s = (dir, steps, "")
  where
    [_, _, color] = words s
    steps = fst $ head $ readHex $ take 5 $ drop 2 color
    dir = words "R D L U" !! (fst $ head $ readHex [last $ take 6 $ drop 2 color])


executePlan :: (Int, Int) -> S.Set (Int, Int) -> [(String, Int, String)] -> S.Set (Int, Int)
executePlan pos inSet [] = inSet
executePlan pos inSet ((dir,steps,_):rest) = executePlan (last positions) outSet rest
  where
    outSet = S.fromList positions `S.union` inSet
    positions = walk dir steps pos

walk :: String -> Int -> (Int, Int) -> [(Int, Int)]
walk dir 0 start = []
walk dir n start = next:walk dir (n-1) next
  where
    next = step dir start
step :: String -> (Int, Int) -> (Int,Int)
step "U" (r,c) = (r-1, c)
step "D" (r,c) = (r+1, c)
step "L" (r,c) = (r, c-1)
step "R" (r,c) = (r, c+1)

showGrid :: S.Set (Int, Int) -> String
showGrid grid = unlines [[if S.member (r,c) grid then '#' else ' ' | c <- [minC..maxC]] | r <- [minR..maxR]]
  where
    ((minR,minC),(maxR,maxC)) = corners grid

corners :: S.Set (Int, Int) -> ((Int,Int), (Int,Int))
corners grid = ((minR,minC),(maxR,maxC))
  where
    minR = minimum $ S.map fst grid
    maxR = maximum $ S.map fst grid
    minC = minimum $ S.map snd grid
    maxC = maximum $ S.map snd grid

data Queue a = Queue Int [a] [a] deriving Show

checkf :: Queue a -> Queue a
checkf (Queue i [] r) = Queue i (reverse r) []
checkf q = q

snoc :: a -> Queue a -> Queue a
snoc x (Queue i f r) = checkf $ Queue (i+1) f (x:r)

isEmpty :: Queue a -> Bool
isEmpty (Queue _ f _) = null f

empty :: Queue a
empty = Queue 0 [] []

front :: Queue a -> a
front (Queue _ f r) = head f

rest :: Queue a -> Queue a
rest (Queue i (x:xs) r) = checkf $ Queue (i-1) xs r

size :: Queue a -> Int
size (Queue i _ _) = i

fillExterior :: S.Set (Int,Int) -> S.Set (Int,Int)
fillExterior grid = fill c (snoc (minR-1,minC-1) empty) grid
  where
    c@((minR,minC),(maxR,maxC)) = corners grid

fill :: ((Int,Int), (Int,Int)) -> Queue (Int,Int) -> S.Set (Int,Int) -> S.Set (Int,Int)
fill corners q s | isEmpty q = s
                 | otherwise = fill corners q' $ S.union s' s
  where
    pos = front q
    q' :: Queue (Int,Int) = foldr snoc (rest q) possible
    possible = filter isValid $ (`step` pos) <$> words "U D L R"
    isValid (r,c) = S.notMember (r,c) s && r >= minR-1 && c >= minC-1 && r <= maxR+1 && c <= maxC+1
    ((minR,minC),(maxR,maxC)) = corners
    s' = S.fromList possible

solve steps =
  let boundary = executePlan (0,0) (S.singleton (0,0)) steps
      exterior = fillExterior boundary
      ((minR,minC),(maxR,maxC)) = corners boundary
  in
    (maxR-minR+3) * (maxC-minC+3) - S.size exterior + S.size boundary

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let steps = parseLine <$> input
  putStrLn "Part 1:"
  -- let boundary = executePlan (0,0) (S.singleton (0,0)) steps
  -- let exterior = fillExterior boundary
  -- let ((minR,minC),(maxR,maxC)) = corners boundary
  -- -- print ((minR,minC),(maxR,maxC))
  -- -- putStrLn $ showGrid exterior
  -- -- print $ (maxR-minR+3) * (maxC-minC+3)
  -- -- print $ (S.size exterior, S.size boundary)
  -- print $ (maxR-minR+3) * (maxC-minC+3) - S.size exterior + S.size boundary
  print $ solve steps
  putStrLn "Part 2:"
  let steps' = parseLine' <$> input
  let vertical = filter (\(d,_,_) -> d == "U" || d == "D") steps'
  print $ foldl1 gcd $ map (\(_,s,_) -> s) vertical
  --print $ solve steps'
