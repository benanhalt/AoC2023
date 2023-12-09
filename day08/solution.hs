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

type Network = M.Map String (String, String)

parseNode :: String -> (String, (String, String))
parseNode s = (node, (left, right))
  where
    (_, _, _, [node, left, right]) :: (String, String, String, [String]) = s =~ "(...) = \\((...), (...)\\)"

step :: Network -> String -> Char -> String
step network current dir = case dir of
  'L' -> left
  'R' -> right
  where
    Just (left, right) = M.lookup current network

run :: Network -> [Char] -> String -> [String]
run network (next:rest) current = current : run network rest (step network current next)

findPath :: Network -> [Char] -> String -> [String]
findPath network leftRight start = takeWhile (not . ("Z" `isSuffixOf`)) $ run network (cycle leftRight) start

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let leftRight = head input
  putStrLn "Part 1:"
  let network = M.fromList $ parseNode <$> drop 2 input
  let path = takeWhile (/= "ZZZ") $ run network (cycle leftRight) "AAA"
  print $ length path
  putStrLn "Part 2:"
  let startingNodes = filter ("A" `isSuffixOf`) $ M.keys network
  print startingNodes
  let paths = findPath network leftRight <$> startingNodes
  print $ foldl' lcm 1 $ length <$> paths

