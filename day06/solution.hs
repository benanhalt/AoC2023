import Data.Char (isDigit)

-- Let t be the given time of the race, and th be the
-- button hold time. Then the speed of the boat is equal
-- to th and the distance covered is th(t-th). If tr is
-- the record time, we want positive values of
-- f(th) = th(t-th) - tr. Since the function is quadratic
-- this corresponds to values of th between the two roots of
-- 0 = -th^2 + t*th - tr. These are
-- th1 = [t - sqrt(t^2 - 4tr)]/2 and
-- th2 = [t + sqrt(t^2 - 4tr)]/2.
-- The distance between the roots is
-- dr = th2 - th1 = sqrt(t^2 - 4tr).

dr :: Double -> Double -> Double
dr t tr = sqrt(t*t - 4.0*tr)


main :: IO ()
main = do
  [timesLn, distancesLn] <- lines <$> readFile "input.txt"
  putStrLn "Part 1:"
  let times :: [Double] = read <$> (tail $ words timesLn)
  let distances :: [Double] = read <$> (tail $ words distancesLn)
  print $ product $ floor <$> zipWith dr times distances

  putStrLn "Part 2:"
  let time :: Double = read $ filter isDigit timesLn
  let distance :: Double = read $ filter isDigit distancesLn
  print $ floor $ dr time distance
