import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Debug.Trace

type Coords = (Int, Int)

data Dir = North | South | East | West
  deriving (Eq, Ord, Show)

data Ray = Ray
  { pos :: Coords,
    dir :: Dir,
    history :: S.Set (Coords, Dir)
  }
  deriving (Show)

type Grid = M.Map Coords Char

traceRay :: Grid -> Ray -> S.Set (Coords, Dir)
traceRay grid ray =
  -- traceShow (S.size $ history ray) $
  if offGrid grid ray' || cycled ray'
    then history ray
    else case updateRay grid ray' of
      Left r -> traceRay grid r
      Right (r1, r2) -> traceRay grid $ r2 {history = traceRay grid r1}
  where
    ray' = ray {pos = nextCoord ray}

offGrid :: Grid -> Ray -> Bool
offGrid grid ray = M.notMember (pos ray) grid

cycled :: Ray -> Bool
cycled Ray {pos, dir, history} = S.member (pos, dir) history

updateRay :: Grid -> Ray -> Either Ray (Ray, Ray)
updateRay grid rayIn@Ray {pos, dir, history} = case tile of
  Just '|' -> case dir of
    North -> Left ray
    South -> Left ray
    East -> Right (ray {dir = North}, ray {dir = South})
    West -> Right (ray {dir = North}, ray {dir = South})
  Just '-' -> case dir of
    East -> Left ray
    West -> Left ray
    North -> Right (ray {dir = East}, ray {dir = West})
    South -> Right (ray {dir = East}, ray {dir = West})
  Just '\\' -> case dir of
    East -> Left $ ray {dir = South}
    West -> Left $ ray {dir = North}
    North -> Left $ ray {dir = West}
    South -> Left $ ray {dir = East}
  Just '/' -> case dir of
    East -> Left $ ray {dir = North}
    West -> Left $ ray {dir = South}
    North -> Left $ ray {dir = East}
    South -> Left $ ray {dir = West}
  _ -> Left ray
  where
    tile = M.lookup pos grid
    ray = rayIn {history = S.insert (pos, dir) history}

nextCoord :: Ray -> Coords
nextCoord Ray {pos, dir} = case dir of
  North -> (r - 1, c)
  South -> (r + 1, c)
  East -> (r, c + 1)
  West -> (r, c - 1)
  where
    (r, c) = pos

showHistory :: S.Set Coords -> String
showHistory hist =
  unlines
    [[if S.member (r, c) hist then '#' else '.' | c <- [0 .. maxC]] | r <- [0 .. maxR]]
  where
    (maxR, maxC) = S.findMax hist

startingRays :: Grid -> [Ray]
startingRays grid =
  [Ray {pos = (r, -1), dir = East, history = S.empty} | r <- [0 .. maxR]]
    ++ [Ray {pos = (r, maxC + 1), dir = West, history = S.empty} | r <- [0 .. maxR]]
    ++ [Ray {pos = (-1, c), dir = South, history = S.empty} | c <- [0 .. maxC]]
    ++ [Ray {pos = (maxR + 1, c), dir = North, history = S.empty} | c <- [0 .. maxC]]
  where
    ((maxR, maxC), _) = M.findMax grid

main :: IO ()
main = do
  rows <- lines <$> readFile "input.txt"
  let grid :: Grid =
        M.fromList
          [((r, c), t) | (r, row) <- zip [0 ..] rows, (c, t) <- zip [0 ..] row]
  putStrLn "Part 1:"
  let result = S.map fst $ traceRay grid Ray {pos = (0, -1), dir = East, history = S.empty}
  -- putStrLn $ showHistory result
  print $ S.size result
  putStrLn "Part 2:"
  let results = S.size . S.map fst <$> map (traceRay grid) (startingRays grid)
  print $ maximum results
