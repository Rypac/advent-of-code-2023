module Main (main) where

import Data.ByteString qualified as B
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import System.Environment (getArgs)
import Text.Read (readMaybe)

{- Types for your input and your solution

- Input    should as the type of your input parameter. AOC, typically uses arrays, matrices or complex data structures.
- Solution should be the type of your solution. Typically is an Int, but It can be other things, like a list of numbers
         or a list of characters
-}
type Input = Game
type Solution = Int

data Color = Red | Green | Blue
  deriving stock (Eq, Ord)

type Game = Map.Map Int [Draw]
type Draw = Map.Map Color Int

-- | parser transforms a raw bytestring (from your ./input/day-X.input) to your Input type.
--   this is intended to use attoparsec for such a transformation. You can use Prelude's
--   String if it fit better for the problem
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

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 = sum . Map.keys . Map.filter (all isValidDraw)
 where
  isValidDraw :: Draw -> Bool
  isValidDraw = all (\(color, value) -> value <= lookupColorValue color) . Map.toList

  lookupColorValue :: Color -> Int
  lookupColorValue = \case
    Red -> 12
    Green -> 13
    Blue -> 14

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 = error "Part 2 Not implemented"

main :: IO ()
main = do
  -- run this with cabal run -- day-x <part-number> <file-to-solution>
  -- example: cabal run -- day-3 2 "./input/day-3.example"
  -- will run part two of day three with input file ./input/day-3.example
  [part, filepath] <- getArgs
  input <- parser <$> B.readFile filepath -- use parser <$> readFile filepath if String is better
  if read @Int part == 1
    then do
      putStrLn "solution to problem 1 is:"
      print $ solve1 input
    else do
      putStrLn "solution to problem 2 is:"
      print $ solve2 input
