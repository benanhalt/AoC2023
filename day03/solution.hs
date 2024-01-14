import Control.Monad (guard)
import Data.Char (isDigit)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set

type Grid = Map.Map (Int, Int) Char

isPartSymbol :: Char -> Bool
isPartSymbol '.' = False
isPartSymbol c   = not $ isDigit c


partNoMap :: (Int, Int) -> Grid -> [(Int, Int)]
partNoMap (maxR, maxC) grid  = do
  r <- [0 .. maxR]
  c <- [0 .. maxC]
  char <- maybeToList $ Map.lookup (r, c) grid
  guard $ isPartSymbol char
  -- (r,c) is now the coordinate of a part symbol
  dr <- [-1, 0, 1]
  dc <- [-1, 0, 1]
  char <- maybeToList $ Map.lookup (r + dr, c + dc) grid
  guard $ isDigit char
  -- have coordinate of a digit adjacent to a part
  expandPartNo grid (r + dr, c + dc)

expandPartNo :: Grid -> (Int, Int) -> [(Int, Int)]
expandPartNo grid (r, c) = left ++ right ++ [(r, c)]
  where
    left = takeWhile (\rc -> isDigit $ Map.findWithDefault '.' rc grid) [(r, c - dc) | dc <- [1 ..]]
    right = takeWhile (\rc -> isDigit $ Map.findWithDefault '.' rc grid) [(r, c + dc) | dc <- [1 ..]]

showGrid :: (Int, Int) -> Grid -> String
showGrid (maxR, maxC) grid = unlines $ do
  r <- [0..maxR]
  pure $ do
    c <- [0..maxC]
    pure $ Map.findWithDefault ' ' (r,c) grid

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let grid :: Grid = Map.fromList $ do
        (r, row) <- zip [0 ..] input
        (c, char) <- zip [0 ..] row
        pure ((r, c), char)
  let Just (maxRC, _) = Map.lookupMax grid
  putStrLn "Part 1:"
  let partNoCoords = Set.fromList $ partNoMap maxRC grid
  let partNosOnly = showGrid maxRC $ Map.filterWithKey (\rc _ -> Set.member rc partNoCoords) grid
  -- putStrLn partNosOnly
  print $ sum $ read <$> words partNosOnly
  putStrLn "Part 2:"
