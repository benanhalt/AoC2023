import Data.Functor
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.List (inits, foldl')
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)


data Rule
  = LTRule Char Int String
  | GTRule Char Int String
  deriving (Show)

data WorkFlow = WorkFlow String [Rule] String
  deriving (Show)


workFlow :: Parser WorkFlow
workFlow = do
  name <- many1 letter
  _ <- char '{'
  rs <- (Right <$> try rule <|> Left <$> many1 letter) `sepBy` char ','
  case sequence $ init rs of
    Left _ -> fail "default before rules"
    Right rules -> case last rs of
      Right _ -> fail "missing default"
      Left def -> do
        _ <- char '}'
        pure $ WorkFlow name rules def

rule :: Parser Rule
rule = do
  var <- letter
  op <- string ">" $> GTRule
    <|> string "<" $> LTRule
  val <- read <$> many1 digit
  _ <- char ':'
  dest <- many1 letter
  pure $ op var val dest


type Part = Map Char Int
type PartVal = (Char, Int)

partVal :: Parser PartVal
partVal = do
  var <- letter
  _ <- char '='
  val <- read <$> many1 digit
  pure (var, val)

part :: Parser Part
part = do
  _ <- char '{'
  vals <- partVal `sepBy` char ','
  pure $ M.fromList vals

parseWF :: String -> WorkFlow
parseWF s = case parse workFlow "" s of
  Left err -> error $ show err ++ s
  Right wf -> wf

parsePart :: String -> Part
parsePart s = case parse part "" s of
  Left err -> error $ show err ++ s
  Right p -> p

applyWF :: WorkFlow -> Part -> String
applyWF (WorkFlow _ rules def) part =
  let
    matchedRules = mapMaybe (`applyRule` part) rules
  in
    if null matchedRules then def else head matchedRules

applyRule :: Rule -> Part -> Maybe String
applyRule (LTRule var val dest) part =
  case M.lookup var part of
    Just v | v < val -> Just dest
    otherwise        -> Nothing

applyRule (GTRule var val dest) part =
  case M.lookup var part of
    Just v | v > val -> Just dest
    otherwise        -> Nothing

run :: Map String WorkFlow -> Part -> String -> String
run wfs part wfName = case M.lookup wfName wfs of
  Nothing -> wfName
  Just wf -> applyWF wf part

runFull :: Map String WorkFlow -> Part -> String
runFull wfs part = head $ dropWhile (not . (`elem` ["A", "R"])) $
  iterate (run wfs part) "in"

inv :: Rule -> Rule
inv (LTRule var val dest) = GTRule var (val - 1) dest
inv (GTRule var val dest) = LTRule var (val + 1) dest

dest :: Rule -> String
dest (LTRule _ _ d) = d
dest (GTRule _ _ d) = d

acceptedRuleChains :: Map String WorkFlow -> String -> [Rule] -> [[Rule]]
acceptedRuleChains _ "A" prevRules = [prevRules]
acceptedRuleChains _ "R" _ = []
acceptedRuleChains wfs startingWF prevRules =
  let
    Just (WorkFlow name rules def) = M.lookup startingWF wfs
    invRules = map inv rules
    conds = zipWith (:) rules (inits invRules) ++ [invRules]
    dests = map dest rules ++ [def]
    prefixedConds = (prevRules ++) <$> conds
  in
    concat $ zipWith (acceptedRuleChains wfs) dests prefixedConds

andRules :: [Rule] -> Map Char (Int, Int)
andRules = foldl' update (M.fromList [('x', (1, 4000)), ('m', (1, 4000)), ('a', (1, 4000)), ('s', (1, 4000))])
  where
    update vars (LTRule var val _) =
      let
        Just (minVal, maxVal) = M.lookup var vars
      in
        M.insert var (minVal, min (val - 1) maxVal) vars
    update vars (GTRule var val _) =
      let
        Just (minVal, maxVal) = M.lookup var vars
      in
        M.insert var (max minVal (val + 1), maxVal) vars

possibilities :: Map Char (Int, Int) -> Int
possibilities vars = product [maxVal - minVal + 1 | (minVal, maxVal) <- M.elems vars]

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let [workflows, ratings] = splitOn [""] input
  let wfs = M.fromList $ do
        wf@(WorkFlow name _ _) <- parseWF <$> workflows
        pure (name, wf)
  let parts = parsePart <$> ratings

  putStrLn "Part 1:"
  let accepted = filter ((== "A") . runFull wfs) parts
  print $ sum $ sum <$> accepted

  putStrLn "Part 2:"
  let result = andRules <$> acceptedRuleChains wfs "in" []
  print $ sum $ possibilities <$> result
