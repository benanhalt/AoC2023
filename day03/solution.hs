import Control.Monad (guard)
import Data.Char (isDigit)
import Data.List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Debug.Trace

type Grid = Map.Map (Int, Int) Char

isPartSymbol :: Char -> Bool
isPartSymbol '.' = False
isPartSymbol c = not $ isDigit c

-- return list of all coordinates that include digits of part numbers
partNoMap :: (Int, Int) -> Grid -> [(Int, Int)]
partNoMap (maxR, maxC) grid = do
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

-- given the coordinate of a digit, return the coordinates of all
-- digits of the part no containing it
expandPartNo :: Grid -> (Int, Int) -> [(Int, Int)]
expandPartNo grid (r, c) = left ++ right ++ [(r, c)]
  where
    left = takeWhile (\rc -> isDigit $ Map.findWithDefault '.' rc grid) [(r, c - dc) | dc <- [1 ..]]
    right = takeWhile (\rc -> isDigit $ Map.findWithDefault '.' rc grid) [(r, c + dc) | dc <- [1 ..]]

-- return the gear ratios of all gears in the grid
findGears :: (Int, Int) -> Grid -> [Int]
findGears (maxR, maxC) grid = do
  r <- [0 .. maxR]
  c <- [0 .. maxC]
  char <- maybeToList $ Map.lookup (r, c) grid
  guard $ char == '*'
  -- (r,c) is now the coordinate of a potential gear
  let partNosWithExtents = do
        dr <- [-1, 0, 1]
        dc <- [-1, 0, 1]
        char <- maybeToList $ Map.lookup (r + dr, c + dc) grid
        guard $ isDigit char
        -- have coordinate of a digit adjacent to gear.
        -- find the extent (coordinates of the digits) of the part number
        let extent = Set.fromList $ expandPartNo grid (r + dr, c + dc)
        -- return the part number and its extent
        pure (expandPartNo' grid (r + dr, c + dc), extent)
  -- remove duplicate part nos by comparing their extents
  -- this preserves duplicate part nos that are not coincidental
  let partNos = fst <$> nubBy (\a b -> snd a == snd b) partNosWithExtents
  guard $ length partNos == 2 -- only a gear if exactly 2 part nos
  pure $ product partNos -- gear ratio is product of the part nos

-- given the coords of a digit return the value of the part no
-- containing it
expandPartNo' :: Grid -> (Int, Int) -> Int
expandPartNo' grid (r, c) = read $ left ++ right
  where
    left = reverse $ takeWhile isDigit [Map.findWithDefault '.' (r, c - dc) grid | dc <- [1 ..]]
    right = takeWhile isDigit [Map.findWithDefault '.' (r, c + dc) grid | dc <- [0 ..]]

showGrid :: (Int, Int) -> Grid -> String
showGrid (maxR, maxC) grid = unlines $ do
  r <- [0 .. maxR]
  pure $ do
    c <- [0 .. maxC]
    pure $ Map.findWithDefault ' ' (r, c) grid

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let grid :: Grid = Map.fromList $ do
        (r, row) <- zip [0 ..] input
        (c, char) <- zip [0 ..] row
        pure ((r, c), char)
  let Just (maxRC, _) = Map.lookupMax grid
  putStrLn "Part 1:"
  -- get coords of all part no digits
  let partNoCoords = Set.fromList $ partNoMap maxRC grid
  -- rebuild the grid with only the part no digits
  let partNosOnly = showGrid maxRC $ Map.filterWithKey (\rc _ -> Set.member rc partNoCoords) grid
  -- can now use words to parse out all the part nos and sum them up
  print $ sum $ read <$> words partNosOnly
  putStrLn "Part 2:"
  print $ sum $ findGears maxRC grid
