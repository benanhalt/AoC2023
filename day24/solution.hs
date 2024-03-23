import Data.List (tails)

type Q = Rational

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x : ys) <- tails l, y <- ys]

intersect :: [Q] -> [Q] -> Maybe (Q, Q, Q, Q)
intersect [x, y, z, vx, vy, vz] [x', y', z', vx', vy', vz'] =
  let d = vx * vy' - vy * vx'
      t = (vx' * (y - y') - vy' * (x - x')) / d
      t' = (vx * (y - y') - vy * (x - x')) / d
      x1 = x' + vx' * t'
      y1 = y' + vy' * t'
   in if abs d > 0 then Just (t, t', x1, y1) else Nothing

test :: Q -> Q -> Maybe (Q, Q, Q, Q) -> Bool
test _ _ Nothing = False
test a b (Just (t, t', x, y)) = t >= 0 && t' >= 0 && x >= a && x <= b && y >= a && y <= b

partOne :: [[Integer]] -> Int
partOne stones = length $ filter (test 200000000000000 400000000000000) $
  map (uncurry intersect) $ pairs $ map toRational <$> stones

-- crossV a returns the matrix m s.t. mMult m b = a X b.
crossV :: Num a => [a] -> [[a]]
crossV [x, y, z] =
    [[ 0, -z,  y],
     [ z,  0, -x],
     [-y,  x,  0]]

-- mMult m v multiplies the column vector v by the matrix m.
mMult :: Num a => [[a]] -> [a] -> [a]
mMult m v = map (sum . zipWith (*) v) m

-- vSub a b returns a - b where a and b are vectors.
vSub :: Num a => [a] -> [a] -> [a]
vSub = zipWith (-)

-- horizontally join mXn and mXo matrices into a mX(n+o) block matrix.
joinHorz :: [[a]] -> [[a]] -> [[a]]
joinHorz = zipWith (++)

-- vertically join mXn and oXn matrices into a (m+o)xn block matrix.
joinVert :: [a] -> [a] -> [a]
joinVert = (++)

-- given three stones, solve for the position and velocity of the
-- thrown stone.
solve :: [[Q]] -> [Q]
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

-- The following three functions are adapted from
-- https://haskellicious.wordpress.com/2012/11/26/the-gauss-algorithm-in-haskell/

-- solve a linear system of the form ax=b for x where
-- a is a matrix and b is a column vector.
geSolve :: [[Q]] -> [Q] -> [Q]
geSolve a b = resubstitute $ triangular a'
  where
    a' = zipWith (++) a $ map pure b

-- use guassian elimination to convert a matrix into
-- upper triangular form.
triangular :: [[Q]] -> [[Q]]
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

-- use back substitution to find the solution to a linear system given
-- by an upper triangular matrix.
resubstitute :: [[Q]] -> [Q]
resubstitute = reverse . resubstitute' . reverse . map reverse
  where
    resubstitute' [] = []
    resubstitute' (row : rows) = x : resubstitute' rows'
      where
        x = (head row)/(last row)
        rows' = map substituteUnknown rows
        substituteUnknown (a1:a2:as) = a1-x*a2:as

partTwo :: [[Integer]] -> Rational
partTwo stones =
  let solution = solve $ map (map toRational) $ take 3 stones
  in sum $ take 3 solution

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let stones = map read . words . filter (`elem` "0123456789 -") <$> input
  print $ partOne stones
  print $ partTwo stones

