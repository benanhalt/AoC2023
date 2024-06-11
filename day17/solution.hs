{-# LANGUAGE DeriveGeneric #-}

import Data.Map.Strict qualified as M
import Data.HashPSQ qualified as Q
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Data.Set qualified as S
import Control.Monad (guard)
import Data.List (unfoldr, group, inits)
import Data.Maybe (mapMaybe)
import Debug.Trace

data WithInf a = Finite a | Infinite
  deriving (Show, Eq, Ord)


data Vertex = MkVertex {
  coord :: !(Int, Int),
  dir :: !Int
  } deriving (Show, Eq, Ord, Generic)

instance Hashable Vertex

data Graph v w = MkGraph {
  vertices :: ![v],
  neighbors :: v -> [(v, w)]
  }

dijkstra :: (Ord v, Show v, Hashable v, Num w, Ord w, Show w) => Graph v w -> v -> (v -> Bool) -> (w, ())
dijkstra graph source fin =
  let
    q0 = Q.fromList [(v, if v == source then Finite 0 else Infinite, ()) | v <- vertices graph]
  in
    loop q0
  where
    loop q = --traceShow (Q.size q) $
      case Q.findMin q of
        Nothing -> error "no path"
        Just (u, Finite c, path)
          | fin u  -> (c, path)
          | otherwise  -> loop q'
          where
            ns = --traceShowId $
              neighbors graph u
            q' = foldr (\(n, w) q -> case Q.lookup n q of
                           Just (Finite d, _) -> if d < w + c then q else Q.insert n (Finite $ w + c) () q
                           Just (Infinite, _) -> Q.insert n (Finite $ w + c) () q
                           Nothing -> q
                       ) (Q.delete u q) ns
        other -> error $ show other

dirs :: [(Int,Int)]
dirs = [(1,0), (0,1), (-1,0), (0,-1)]

getNeighbors :: M.Map (Int, Int) Int -> Int -> Int -> Vertex -> [(Vertex, Int)]
getNeighbors grid minStep maxStep MkVertex { coord, dir } = do
  let (r,c) = coord
  dir' <- [(dir + 1) `rem` 4, (dir + 3) `rem` 4]
  let (dr, dc) = dirs !! dir'
  steps <- [minStep..maxStep]
  let coords = [(r + s*dr, c + s*dc) | s <- [1..steps]]
  let costs = mapMaybe (`M.lookup` grid) coords
  let totalCosts = tail $ map sum $ inits costs
  drop (minStep - 1) $ zipWith (\c w -> (MkVertex { coord = c, dir = dir' }, w)) coords totalCosts

allowed ::[(Int, Int)] ->  Bool
allowed path =
  let
    deltas = zipWith (\(r',c') (r,c) -> (r'-r, c'-c)) path (tail path)
  in traceShow path $
    null deltas || (length (take 5 $ head $ group deltas) < 4)

main :: IO ()
main = do
  rows <- lines <$> readFile "input.txt"
  let grid :: M.Map (Int, Int) Int = M.fromList $ do
        (r, row) <- zip [0..] rows
        (c, value) <- zip [0..] row
        return ((r,c), read [value])
  let Just ((rs, cs), _) = M.lookupMax grid

  putStrLn "Part 1:"
  let graph = MkGraph {
        vertices = [MkVertex { coord = coord, dir = dir } | coord <- M.keys grid, dir <- [0..3]],
        neighbors = getNeighbors grid 1 3
        }
  let (w, vs) = dijkstra graph (MkVertex { coord = (0,0), dir = 0 }) (\MkVertex {coord} -> coord == (rs, cs))
  print w

  putStrLn "Part 2:"
  let graph' = graph { neighbors = getNeighbors grid 4 10 }
  let (w, vs) = dijkstra graph' (MkVertex { coord = (0,0), dir = 0 }) (\MkVertex {coord} -> coord == (rs, cs))
  print w
