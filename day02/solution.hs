import Data.Functor
import Text.Parsec
import Text.Parsec.String (Parser)

data Color = Red | Green | Blue deriving (Eq, Show)

{-
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
-}

color :: Parser Color
color =
  string "red" $> Red
    <|> string "green" $> Green
    <|> string "blue" $> Blue

number :: Parser Int
number = read <$> many1 digit

cubeCount :: Parser (Int, Color)
cubeCount = do
  n <- number
  _ <- string " "
  c <- color
  pure (n, c)

type Handful = [(Int, Color)]

handful :: Parser Handful
handful = cubeCount `sepBy` string ", "

type Game = (Int, [Handful])

game :: Parser Game
game = do
  _ <- string "Game "
  n <- number
  _ <- string ": "
  handfuls <- handful `sepBy` string "; "
  _ <- char '\n'
  pure (n, handfuls)

minColors :: Game -> (Int, Int, Int)
minColors (_, handfuls) = foldl (foldl reduce) (0, 0, 0) handfuls
  where
    reduce (rs, gs, bs) (n, Red) = (max rs n, gs, bs)
    reduce (rs, gs, bs) (n, Green) = (rs, max gs n, bs)
    reduce (rs, gs, bs) (n, Blue) = (rs, gs, max bs n)

possible :: Game -> Bool
possible game = reds <= 12 && greens <= 13 && blues <= 14
  where
    (reds, greens, blues) = minColors game

power :: (Int, Int, Int) -> Int
power (r, g, b) = r * g * b

main :: IO ()
main = do
  let fname = "input.txt"
  input <- readFile fname
  let games = case parse (many game) fname input of
        Left err -> error $ show err
        Right games -> games
  putStrLn "Part 1:"
  print $ sum $ fst <$> filter possible games
  putStrLn "Part 2:"
  print $ sum $ power . minColors <$> games
