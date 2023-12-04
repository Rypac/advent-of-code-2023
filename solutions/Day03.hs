module Main (main) where

import Data.ByteString qualified as B
import Data.Foldable (foldl')
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Void (Void)
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Input = [Parsed]
type Solution = Int

data Parsed
  = Number !Int !Pos !Pos
  | Symbol !Char !Pos !Pos
  deriving stock (Show)

type Parser = Parsec Void Text

parser :: String -> B.ByteString -> Input
parser filepath = parseOrError engineSchematicParser filepath . decodeUtf8
 where
  parseOrError :: Parser a -> String -> Text -> a
  parseOrError p path = either (error . errorBundlePretty) id . parse p path

  engineSchematicParser :: Parser [Parsed]
  engineSchematicParser = many numberOrSymbolParser <* eof

  numberOrSymbolParser :: Parser Parsed
  numberOrSymbolParser = do
    separatorConsumer
    L.lexeme separatorConsumer (numberParser <|> symbolParser)

  separatorConsumer :: Parser ()
  separatorConsumer = skipMany (char '.' <|> newline)

  numberParser :: Parser Parsed
  numberParser = do
    value <- L.decimal
    pos <- getSourcePos
    return $ Number value (sourceLine pos) (sourceColumn pos)

  symbolParser :: Parser Parsed
  symbolParser = do
    value <- char '*' <|> char '#' <|> char '@' <|> char '$' <|> char '%' <|> char '&' <|> char '=' <|> char '+' <|> char '/' <|> char '-'
    pos <- getSourcePos
    return $ Symbol value (sourceLine pos) (sourceColumn pos)

part1 :: Input -> Solution
part1 input =
  let
    symbolLocations = Set.fromList $ mapMaybe symbolLocation input
   in
    sum $ mapMaybe (maybePartNumber symbolLocations) input
 where
  maybePartNumber :: Set.Set (Int, Int) -> Parsed -> Maybe Int
  maybePartNumber symbols = \case
    Number value line col ->
      let
        digitCount = digits value
        x = unPos col
        y = unPos line
        cols = [x - (digitCount + 1) .. x]
        lines = [y - 1 .. y + 1]
        locations = concatMap (\c -> map (\l -> (c, l)) lines) cols
       in
        if any (`Set.member` symbols) locations
          then Just value
          else Nothing
    Symbol _ _ _ -> Nothing

  digits :: Int -> Int
  digits 0 = 0
  digits n = 1 + digits (div n 10)

  symbolLocation :: Parsed -> Maybe (Int, Int)
  symbolLocation = \case
    Symbol _ line col -> Just ((unPos col) - 1, unPos line)
    Number _ _ _ -> Nothing

part2 :: Input -> Solution
part2 input =
  let
    gearSymbolLocations = mapMaybe gearSymbolLocation input
    partNumbers = fmap (\symbol -> mapMaybe (partNumber symbol) input) gearSymbolLocations
   in
    sum $ fmap gearRatio $ filter isGear partNumbers
 where
  partNumber :: (Int, Int) -> Parsed -> Maybe Int
  partNumber gearSymbol = \case
    Number value line col ->
      let
        digitCount = digits value
        x = unPos col
        y = unPos line
        cols = [x - (digitCount + 1) .. x]
        lines = [y - 1 .. y + 1]
        locations = concatMap (\c -> map (\l -> (c, l)) lines) cols
       in
        if any (\l -> l == gearSymbol) locations
          then Just value
          else Nothing
    Symbol _ _ _ -> Nothing

  isGear :: [Int] -> Bool
  isGear partNumbers = length partNumbers == 2

  gearRatio :: [Int] -> Int
  gearRatio = foldl' (*) 1

  digits :: Int -> Int
  digits 0 = 0
  digits n = 1 + digits (div n 10)

  gearSymbolLocation :: Parsed -> Maybe (Int, Int)
  gearSymbolLocation = \case
    Symbol '*' line col -> Just ((unPos col) - 1, unPos line)
    Symbol _ _ _ -> Nothing
    Number _ _ _ -> Nothing

main :: IO ()
main = do
  [part, filepath] <- getArgs
  input <- parser filepath <$> B.readFile filepath
  if read @Int part == 1
    then do
      putStrLn "solution to problem 1 is:"
      print $ part1 input
    else do
      putStrLn "solution to problem 2 is:"
      print $ part2 input
