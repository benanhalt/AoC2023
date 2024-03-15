import Data.List (tails)

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

partOne :: Real a => [[a]] -> Int
partOne stones = length $ filter (test 200000000000000 400000000000000) $
  map (uncurry intersect) $ pairs $ map toRational <$> stones

crossV :: Num a => [a] -> [[a]]
crossV [x, y, z] =
    [[ 0, -z,  y],
     [ z,  0, -x],
     [-y,  x,  0]]

mMult :: Num a => [[a]] -> [a] -> [a]
mMult m v = map (sum . zipWith (*) v) m

vSub :: Num a => [a] -> [a] -> [a]
vSub = zipWith (-)

joinHorz :: [[a]] -> [[a]] -> [[a]]
joinHorz = zipWith (++)

joinVert :: [a] -> [a] -> [a]
joinVert = (++)

solve :: [[Rational]] -> [Rational]
solve [s1, s2, s3] =
  let (r1, v1) = splitAt 3 s1
      (r2, v2) = splitAt 3 s2
      (r3, v3) = splitAt 3 s3
      rhs = joinVert
        ((crossV v1 `mMult` r1) `vSub` (crossV v2 `mMult` r2))
        ((crossV v1 `mMult` r1) `vSub` (crossV v3 `mMult` r3))
      coeff = joinVert
        (joinHorz (crossV $ v1 `vSub` v2) (crossV $ r1 `vSub` r2))
        (joinHorz (crossV $ v1 `vSub` v3) (crossV $ r1 `vSub` r3))
   in geSolve coeff rhs

geSolve :: (Fractional a, Eq a) => [[a]] -> [a] -> [a]
geSolve a b = resubstitute $ triangular a'
  where
    a' = zipWith (++) a $ map pure b

triangular :: (Fractional a, Eq a) => [[a]] -> [[a]]
triangular [] = []
triangular m = row : triangular rows'
  where
    (row : rows) = rotatePivot m
    rows' = map f rows
    f bs | head bs == 0 = drop 1 bs
         | otherwise    = drop 1 $ zipWith (-) (map (*c) bs) row
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

partTwo :: Real a => [[a]] -> Rational
partTwo stones =
  let solution = solve $ map (map toRational) $ take 3 stones
  in sum $ take 3 solution

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let stones = map read . words . filter (`elem` "0123456789 -") <$> input
  print $ partOne stones
  print $ partTwo stones

