import Data.List (tails)
import Data.List.Split (chunksOf)
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x : ys) <- tails l, y <- ys]

intersect :: (Fractional a, Ord a) => [a] -> [a] -> Maybe (a, a, a, a)
intersect [x, y, z, vx, vy, vz] [x', y', z', vx', vy', vz'] =
  let d = vx * vy' - vy * vx'
      t = (vx' * (y - y') - vy' * (x - x')) / d
      t' = (vx * (y - y') - vy * (x - x')) / d
      x1 = x' + vx' * t'
      y1 = y' + vy' * t'
   in if abs d > 0 then Just (t, t', x1, y1) else Nothing

test :: (Ord a, Num a) => a -> a -> Maybe (a, a, a, a) -> Bool
test _ _ Nothing = False
test a b (Just (t, t', x, y)) = t >= 0 && t' >= 0 && x >= a && x <= b && y >= a && y <= b

crossV :: Vector Z -> Matrix Z
crossV v =
  let
    [x, y, z] = toList v
  in
    (3><3)
    [ 0, -z,  y,
      z,  0, -x,
     -y,  x,  0 ]

solve :: [[Z]] -> [Rational]
solve [s1, s2, s3] =
  let r1 = fromList $ take 3 s1
      r2 = fromList $ take 3 s2
      r3 = fromList $ take 3 s3
      v1 = fromList $ drop 3 s1
      v2 = fromList $ drop 3 s2
      v3 = fromList $ drop 3 s3
      rhs =
        map toRational $ toList $ vjoin
          [ (v1 `cross` r1) - (v2 `cross` r2),
            (v1 `cross` r1) - (v3 `cross` r3)
          ]
      coeff :: [[Rational]] = map (map toRational) $ toLists $ fromBlocks
        [[crossV (v1 - v2), crossV (r1 - r2)],
         [crossV (v1 - v3), crossV (r1 - r3)]]
   in gauss coeff rhs

gauss :: (Fractional a, Eq a) => [[a]] -> [a] -> [a]
gauss a b = resubstitute $ triangular a'
  where
    a' = zipWith (++) a $ map pure b

triangular :: (Fractional a, Eq a) => [[a]] -> [[a]]
triangular [] = []
triangular m = row : triangular rows'
  where
    (row : rows) = rotatePivot m
    rows' = map f rows
    f bs | (head bs) == 0 = drop 1 bs
         | otherwise      = drop 1 $ zipWith (-) (map (*c) bs) row
           where c = (head row)/(head bs)
    rotatePivot m@(row : rows) | head row /= 0 = m
                               | otherwise     = rotatePivot (rows ++ [row])

resubstitute :: (Fractional a) => [[a]] -> [a]
resubstitute = reverse . resubstitute' . reverse . map reverse
  where
    resubstitute' [] = []
    resubstitute' (row : rows) = x : resubstitute' rows'
      where
        x = (head row)/(last row)
        rows' = map substituteUnknown rows
        substituteUnknown (a1:a2:as) = a1-x*a2:as

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let stones = map read . words . filter (`elem` "0123456789 -") <$> input
  print $ length $ filter (test 200000000000000 400000000000000) $ map (uncurry intersect) $ pairs $ map toRational <$> stones
  let solution =  solve $ take 3 stones
  print $ sum $ take 3 solution

