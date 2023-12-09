{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (guard)
import Data.List
import Data.Text (pack, unpack, replace)
import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Debug.Trace (traceShow, traceShowId)
import Text.Regex.PCRE ((=~), getAllTextMatches)

extrapolate :: [Int] -> Int
extrapolate ns = if all (== 0) ns then 0 else
  last ns + extrapolate (derivative ns)

extrapolate' :: [Int] -> Int
extrapolate' ns = if all (== 0) ns then 0 else
  head ns - extrapolate' (derivative ns)

derivative :: [Int] -> [Int]
derivative ns = zipWith (-) (tail ns) ns

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let histories :: [[Int]] = map read . words <$> input
  putStrLn "Part 1:"
  let extrapolated = map extrapolate histories
  print $ sum extrapolated
  putStrLn "Part 2:"
  let extrapolated' = map extrapolate' histories
  print $ sum extrapolated'

