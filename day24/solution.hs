
import Data.List (tails)

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

intersect [x,y,z,vx,vy,vz] [x',y',z',vx',vy',vz'] =
  let
    d = vx*vy'-vy*vx'
    t = (vx'*(y-y') - vy'*(x-x'))/d
    t' = (vx*(y-y') - vy*(x-x'))/d
    x1 = x' + vx'*t'
    y1 = y' + vy'*t'
  in if abs d > 0 then Just (t, t', x1, y1) else Nothing

test :: (Ord a, Num a) => a -> a -> Maybe (a, a, a, a) -> Bool
test _ _ Nothing = False
test a b (Just (t, t', x, y)) = t >= 0 && t' >= 0 && x >= a && x <= b && y >= a && y <= b

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let stones = map read . words . filter (`elem` "0123456789 -") <$> input
  print $ length $ filter (test 200000000000000 400000000000000) $ map (uncurry intersect) $ pairs stones
