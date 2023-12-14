{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (mplus)
import Data.List
import Data.List.Split (splitOn)


-- Count the number of differences between an image and its
-- reflection assuming the mirror is placed after row n.
countDiffs :: Eq a => [[a]] -> Int -> Int
countDiffs puzzle n = sum $ countDiffsInRows <$> zip (reverse image) reflection
  where
    (image, reflection) = splitAt n puzzle
    countDiffsInRows :: Eq a => ([a], [a]) -> Int
    countDiffsInRows = length . filter (uncurry (/=)) . uncurry zip

-- Find a position for the mirror that is consistent with the given
-- number of smudges.
findWithSmudges :: Eq a => Int -> [[a]] -> Maybe Int
findWithSmudges nSmudges puzzle = find ((== nSmudges) . countDiffs puzzle) [1 .. nRows - 1]
  where
    nRows = length puzzle

-- Score a puzzle according to the rules using the given mirror
-- finding function.
scorePuzzle :: Eq a => ([[a]] -> Maybe Int) -> [[a]] -> Maybe Int
scorePuzzle findMirror puzzle = mplus horizontalScore verticalScore
  where
    horizontalScore = (* 100) <$> findMirror puzzle
    -- Finding a vertical mirror is the same as finding a horizontal
    -- mirror in the transpose of the puzzle.
    verticalScore = findMirror (transpose puzzle)

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let puzzles = splitOn [""] input
  putStrLn "Part 1:"
  print $ sum <$> mapM (scorePuzzle (findWithSmudges 0)) puzzles
  putStrLn "Part 2:"
  print $ sum <$> mapM (scorePuzzle (findWithSmudges 1)) puzzles
