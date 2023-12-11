{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (guard)
import Data.List
import Data.Set qualified as S

main :: IO ()
main = do
  rows <- lines <$> readFile "input.txt"
  let galaxies = S.fromList $ do
        (rowN, row) <- zip [0 ..] rows
        (colN, cell) <- zip [0 ..] row
        guard $ cell == '#'
        pure (rowN, colN)
  let nRows = length rows
  let nCols = length $ head rows
  let emptyRows = filter (\row -> all (\col -> (row, col) `S.notMember` galaxies) [0 .. nCols - 1]) [0 .. nRows - 1]
  let emptyCols = filter (\col -> all (\row -> (row, col) `S.notMember` galaxies) [0 .. nRows - 1]) [0 .. nCols - 1]

  putStrLn "Part 1:"
  let adjustedGalaxies = adjustGalaxy 1 emptyRows emptyCols `S.map` galaxies
  print $ sum $ distances adjustedGalaxies

  putStrLn "Part 2:"
  let adjustedGalaxies' = adjustGalaxy (1000000 - 1) emptyRows emptyCols `S.map` galaxies
  print $ sum $ distances adjustedGalaxies'

distances :: S.Set (Int, Int) -> [Int]
distances galaxies = do
  ((row, col) : rest) <- tails $ S.toList galaxies
  (row', col') <- rest
  pure $ abs (row' - row) + abs (col' - col)

adjustGalaxy :: Int -> [Int] -> [Int] -> (Int, Int) -> (Int, Int)
adjustGalaxy scale emptyRows emptyCols (row, col) = (row + rowAd, col + colAd)
  where
    rowAd = scale * length (filter (< row) emptyRows)
    colAd = scale * length (filter (< col) emptyCols)
