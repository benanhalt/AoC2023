import Data.Char
import Data.List
import Data.List.Split (splitOn)
import Data.Map.Strict qualified as M
import Debug.Trace

hash :: Int -> String -> Int
hash h "" = h
hash h (c : rest) = hash (17 * (h + ord c) `rem` 256) rest

data LensInBox = LensInBox
  { label :: String,
    focalLength :: Int
  }
  deriving (Eq, Ord, Show)

type Box = [LensInBox]

type System = M.Map Int Box

step :: System -> String -> System
step sys operation = M.insert boxN (applyOperation label operation box) sys
  where
    label = takeWhile isAlpha operation
    boxN = hash 0 label
    box = M.findWithDefault [] boxN sys

applyOperation :: String -> String -> Box -> Box
applyOperation label' operation box =
  if "-" `isSuffixOf` operation
  then filter ((label' /=) . label) box -- Remove lens with given label.
  else case findIndex ((label' ==) . label) box of -- Find index of labeled lens in box.
    Just i -> take i box ++ [LensInBox {label = label', focalLength}] ++ drop (i + 1) box -- Replace lens at index if found, or
    Nothing -> box ++ [LensInBox {label = label', focalLength}] -- add lens at end.
  where
    focalLength :: Int = read $ filter isDigit operation -- Parse operation for focal length.


score :: System -> Int
score = sum . M.mapWithKey scoreBox

scoreBox :: Int -> Box -> Int
scoreBox boxN box = sum $ zipWith (scoreLens boxN) [1 ..] box

scoreLens :: Int -> Int -> LensInBox -> Int
scoreLens boxN slot LensInBox {label, focalLength} = (boxN + 1) * slot * focalLength

main :: IO ()
main = do
  input <- readFile "input.txt"
  let steps = splitOn "," $ filter (/= '\n') input
  putStrLn "Part 1:"
  print $ sum $ hash 0 <$> steps
  putStrLn "Part 2:"
  let result = foldl' step M.empty steps
  print $ score result
