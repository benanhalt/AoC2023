
import Data.Char (isDigit)
import Data.List (sortOn)


allCards = "J23456789TQKA"

cardStrength :: Char -> Int
cardStrength c | isDigit c = read [c]
cardStrength 'T' = 10
cardStrength 'J' = 11
cardStrength 'Q' = 12
cardStrength 'K' = 13
cardStrength 'A' = 14

cardStrength' :: Char -> Int
cardStrength' c | isDigit c = read [c]
cardStrength' 'T' = 10
cardStrength' 'J' = 1
cardStrength' 'Q' = 11
cardStrength' 'K' = 12
cardStrength' 'A' = 13

data HandType =
  HighCard |
  OnePair |
  TwoPair |
  ThreeOfKind |
  FullHouse |
  FourOfKind |
  FiveOfKind
  deriving (Show, Eq, Ord)

handType :: String -> String -> HandType
handType allCards hand =
  if any (== 5) counts then FiveOfKind
  else if any (== 4) counts then FourOfKind
  else if any (== 3) counts then
    if any (== 2) counts then FullHouse
    else ThreeOfKind
  else if (length $ filter (== 2) counts) == 2 then TwoPair
  else if any (== 2) counts then OnePair
  else HighCard
  where
    counts = length . (\c -> filter (== c) hand) <$> allCards

handType' :: String -> HandType
handType' hand =
  let
    counts = length . (\c -> filter (== c) hand) <$> allCards
    nJ = head counts
    initType = handType (tail allCards) hand
  in
    case nJ of
      0 -> initType
      1 -> case initType of
        HighCard -> OnePair
        OnePair -> ThreeOfKind
        TwoPair -> FullHouse
        ThreeOfKind -> FourOfKind
        FourOfKind -> FiveOfKind
        FiveOfKind -> FiveOfKind
      2 -> case initType of
        HighCard -> ThreeOfKind
        OnePair -> FourOfKind
        TwoPair -> FourOfKind
        ThreeOfKind -> FiveOfKind
        FourOfKind -> FiveOfKind
        FiveOfKind -> FiveOfKind
      3 -> case initType of
        HighCard -> FourOfKind
        _ -> FiveOfKind
      _ -> FiveOfKind

handStrength :: String -> (HandType, [Int])
handStrength hand = (handType allCards hand, cardStrength <$> hand)

handStrength' :: String -> (HandType, [Int])
handStrength' hand = (handType'  hand, cardStrength' <$> hand)

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let hands :: [(String, Int)] = (\[hand, bid] -> (hand, read bid)) . words <$> input
  putStrLn "Part 1:"
  print $ sum $ zipWith (\(_, bid) rank -> bid * rank) (sortOn (handStrength . fst) hands) [1..]
  putStrLn "Part 2:"
  print $ sum $ zipWith (\(_, bid) rank -> bid * rank) (sortOn (handStrength' . fst) hands) [1..]
