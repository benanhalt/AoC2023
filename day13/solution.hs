{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (mplus)
import Data.List
import Data.List.Split (splitOn)

-- isMirrored :: Eq a => [a] -> Int -> Bool
-- isMirrored as n = and $ zipWith (==) (reverse start) end
--   where (start, end) = splitAt n as

countDiffs :: Eq a => [[a]] -> Int -> Int
countDiffs as n = sum $ (\(as, bs) -> length $ filter id $ zipWith (/=) as bs) <$> zip (reverse start) end
  where (start, end) = splitAt n as

-- findMirror :: Eq a => [a] -> Maybe Int
-- findMirror as = find (isMirrored as)  [1..length as - 1]

findWithSmudges :: Eq a => Int -> [[a]] -> Maybe Int
findWithSmudges nSmudges as = find ((== nSmudges) . countDiffs as)  [1..length as - 1]

scorePuzzle :: Eq a => ([[a]] -> Maybe Int) -> [[a]] -> Maybe Int
scorePuzzle findMirror as = mplus horizontal vertical
  where
    horizontal = (100 *) <$> findMirror as
    vertical = findMirror (transpose as)

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let puzzles = splitOn [""] input
  putStrLn "Part 1:"
  print $ sum <$> mapM (scorePuzzle (findWithSmudges 0)) puzzles
  putStrLn "Part 2:"
  print $ sum <$> mapM (scorePuzzle (findWithSmudges 1)) puzzles
