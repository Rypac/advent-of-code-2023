module Main (main) where

import Data.ByteString qualified as B
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Void (Void)
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Input = Almanac
type Solution = Int

data Almanac = Almanac
  { seeds :: ![Int]
  , mappings :: ![Category]
  }
  deriving stock (Show)

type Category = [Mapping]

data Mapping = Mapping
  { source :: !Int
  , range :: !Int
  , destination :: !Int
  }
  deriving stock (Show)

type Parser = Parsec Void Text

parser :: String -> B.ByteString -> Input
parser filepath = parseOrError almanac filepath . decodeUtf8
 where
  parseOrError :: Parser a -> String -> Text -> a
  parseOrError p path = either (error . errorBundlePretty) id . parse p path

  almanac :: Parser Almanac
  almanac = Almanac <$> seeds <*> (many mappings)

  seeds :: Parser [Int]
  seeds = do
    string "seeds:" *> space
    number `sepBy` space

  mappings :: Parser [Mapping]
  mappings = do
    _ <- skipManyTill anySingle newline
    many mapping

  mapping :: Parser Mapping
  mapping = do
    destination <- number
    source <- number
    range <- number
    pure $ Mapping {..}

  number :: Parser Int
  number = L.lexeme space L.decimal

part1 :: Input -> Solution
part1 almanac = minimum $ fmap (location almanac.mappings) almanac.seeds

part2 :: Input -> Solution
part2 almanac = minimum $ fmap (location almanac.mappings) seeds
 where
  seeds :: [Int]
  seeds = concatMap seedRange $ pairs almanac.seeds

  seedRange :: (Int, Int) -> [Int]
  seedRange (start, range) = [start .. start + range - 1]

  pairs :: [Int] -> [(Int, Int)]
  pairs [] = []
  pairs (x : y : xs) = (x, y) : pairs xs

location :: [Category] -> Int -> Int
location [] seed = seed
location (category : rest) seed = location rest (convert category seed)

convert :: [Mapping] -> Int -> Int
convert [] seed = seed
convert (Mapping {source, range, destination} : rest) seed =
  if seed >= source && seed < (source + range)
    then destination + (seed - source)
    else convert rest seed

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
