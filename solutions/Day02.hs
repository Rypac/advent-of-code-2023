module Main (main) where

import Data.ByteString qualified as B
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import System.Environment (getArgs)
import Text.Read (readMaybe)

type Input = Game
type Solution = Int

data Color = Red | Green | Blue
  deriving stock (Eq, Ord)

type Game = Map.Map Int [Draw]
type Draw = Map.Map Color Int

parser :: B.ByteString -> Input
parser = fromJust . parseGames . decodeUtf8
 where
  parseGames :: T.Text -> Maybe Game
  parseGames = fmap Map.fromList . sequenceA . fmap parseGame . filter (not . T.null) . T.lines

  parseGame :: T.Text -> Maybe (Int, [Draw])
  parseGame text = do
    let [gameHeader, gameDraws] = T.splitOn ":" text
    let [_, numberText] = T.splitOn " " gameHeader
    number <- readMaybe $ T.unpack numberText
    draws <- parseDraws gameDraws
    Just (number, draws)

  parseDraws :: T.Text -> Maybe [Draw]
  parseDraws = sequenceA . fmap parseDraw . T.splitOn ";" . T.strip

  parseDraw :: T.Text -> Maybe Draw
  parseDraw = fmap Map.fromList . sequenceA . fmap parseColorCount . T.splitOn "," . T.strip

  parseColorCount :: T.Text -> Maybe (Color, Int)
  parseColorCount text = do
    let [countText, colorText] = T.splitOn " " $ T.strip text
    count <- readMaybe $ T.unpack countText
    color <- parseColor colorText
    Just (color, count)

  parseColor :: T.Text -> Maybe Color
  parseColor = \case
    "blue" -> Just Blue
    "green" -> Just Green
    "red" -> Just Red
    _ -> Nothing

part1 :: Input -> Solution
part1 = sum . Map.keys . Map.filter (all isValidDraw)
 where
  isValidDraw :: Draw -> Bool
  isValidDraw = all (\(color, value) -> value <= lookupColorValue color) . Map.toList

  lookupColorValue :: Color -> Int
  lookupColorValue = \case
    Red -> 12
    Green -> 13
    Blue -> 14

part2 :: Input -> Solution
part2 = sum . fmap power . minDraws
 where
  power :: Map.Map Color Int -> Int
  power = Map.foldl' (*) 1

  minDraws :: Input -> [Map.Map Color Int]
  minDraws = fmap (Map.fromListWith max . concatMap Map.toList) . Map.elems

main :: IO ()
main = do
  [part, filepath] <- getArgs
  input <- parser <$> B.readFile filepath
  if read @Int part == 1
    then do
      putStrLn "solution to problem 1 is:"
      print $ part1 input
    else do
      putStrLn "solution to problem 2 is:"
      print $ part2 input
