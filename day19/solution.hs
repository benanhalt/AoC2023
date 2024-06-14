import Data.Functor
import Text.Parsec
import Text.Parsec.String (Parser)
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

-- px{a<2006:qkq,m>2090:A,rfg}
-- pv{a>1716:R,A}
-- lnx{m>1548:A,A}
-- rfg{s<537:gd,x>2440:R,A}
-- qs{s>3448:A,lnx}
-- qkq{x<1416:A,crn}
-- crn{x>2662:A,R}
-- in{s<1351:px,qqz}
-- qqz{s>2770:qs,m<1801:hdj,R}
-- gd{a>3333:R,R}
-- hdj{m>838:A,pv}

-- {x=787,m=2655,a=1222,s=2876}
-- {x=1679,m=44,a=2067,s=496}
-- {x=2036,m=264,a=79,s=2244}
-- {x=2461,m=1339,a=466,s=291}
-- {x=2127,m=1623,a=2188,s=1013}

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

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let [workflows, ratings] = splitOn [""] input
  let wfs = M.fromList $ do
        wf@(WorkFlow name _ _) <- parseWF <$> workflows
        pure (name, wf)

  let parts = parsePart <$> ratings
  let accepted = filter ((== "A") . runFull wfs) parts
  print $ sum $ sum <$> accepted
