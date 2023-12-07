module Main (main) where

import Data.ByteString qualified as B
import Data.Foldable (foldl')
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Void (Void)
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Input = [Race]
type Solution = Int

data Race = Race
  { time :: !Int
  , duration :: !Int
  }
  deriving stock (Show)

type Parser = Parsec Void Text

racesParser :: String -> B.ByteString -> Input
racesParser filepath = parseOrError races filepath . decodeUtf8
 where
  races :: Parser [Race]
  races = do
    times <- string "Time:" *> space *> L.decimal `sepBy` hspace <* eol
    distances <- string "Distance:" *> space *> L.decimal `sepBy` hspace <* eol
    pure $ fmap (uncurry Race) $ zip times distances

raceParser :: String -> B.ByteString -> Race
raceParser filepath = parseOrError race filepath . decodeUtf8
 where
  race :: Parser Race
  race = do
    times <- string "Time:" *> space *> (some digitChar) `sepBy` hspace <* eol
    distances <- string "Distance:" *> space *> (some digitChar) `sepBy` hspace <* eol

    let time = read $ mconcat times
    let duration = read $ mconcat distances
    pure $ Race {..}

parseOrError :: Parser a -> String -> Text -> a
parseOrError p path = either (error . errorBundlePretty) id . parse p path

part1 :: [Race] -> Solution
part1 = foldl' (*) 1 . fmap winScenarios

part2 :: Race -> Solution
part2 = winScenarios

winScenarios :: Race -> Int
winScenarios race = length $ filter (> race.duration) $ raceScenarios race.time

raceScenarios :: Int -> [Int]
raceScenarios time = race time
 where
  race :: Int -> [Int]
  race 0 = [0]
  race hold = (hold * (time - hold)) : race (hold - 1)

main :: IO ()
main = do
  [part, filepath] <- getArgs
  if read @Int part == 1
    then do
      input <- racesParser filepath <$> B.readFile filepath
      putStrLn "solution to problem 1 is:"
      print $ part1 input
    else do
      input <- raceParser filepath <$> B.readFile filepath
      putStrLn "solution to problem 2 is:"
      print $ part2 input
