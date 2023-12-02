module Main (main) where

import Control.Applicative (asum)
import Data.ByteString qualified as B
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import System.Environment (getArgs)

{- Types for your input and your solution

- Input    should as the type of your input parameter. AOC, typically uses arrays, matrices or complex data structures.
- Solution should be the type of your solution. Typically is an Int, but It can be other things, like a list of numbers
         or a list of characters
-}
type Input = [T.Text]
type Solution = Int

-- | parser transforms a raw bytestring (from your ./input/day-X.input) to your Input type.
--   this is intended to use attoparsec for such a transformation. You can use Prelude's
--   String if it fit better for the problem
parser :: B.ByteString -> Input
parser = T.lines . decodeUtf8

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 = sum . fmap extractNumber
 where
  extractNumber :: T.Text -> Int
  extractNumber line =
    let
      firstDigit = extractDigit line
      lastDigit = extractDigit $ T.reverse line
     in
      firstDigit * 10 + lastDigit

  extractDigit :: T.Text -> Int
  extractDigit line = case (T.uncons line) of
    Just (first, rest) -> case first of
      '0' -> 0
      '1' -> 1
      '2' -> 2
      '3' -> 3
      '4' -> 4
      '5' -> 5
      '6' -> 6
      '7' -> 7
      '8' -> 8
      '9' -> 9
      _ -> extractDigit rest
    Nothing -> 0

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 = sum . mapMaybe extractNumber
 where
  extractNumber :: T.Text -> Maybe Int
  extractNumber line = do
    firstDigit <- extractFirstDigit line
    lastDigit <- extractLastDigit line
    return $ firstDigit * 10 + lastDigit

  extractFirstDigit :: T.Text -> Maybe Int
  extractFirstDigit = asum . fmap digit . T.tails
   where
    digit :: T.Text -> Maybe Int
    digit line = fmap snd $ find (\a -> fst a `T.isPrefixOf` line) digitMappings

  extractLastDigit :: T.Text -> Maybe Int
  extractLastDigit = asum . fmap digit . reverseTails
   where
    digit :: T.Text -> Maybe Int
    digit line = fmap snd $ find (\a -> fst a `T.isSuffixOf` line) digitMappings

    reverseTails :: T.Text -> [T.Text]
    reverseTails text
      | T.null text = [T.empty]
      | otherwise = text : reverseTails (T.init text)

  digitMappings :: [(T.Text, Int)]
  digitMappings =
    [ ("0", 0)
    , ("1", 1)
    , ("2", 2)
    , ("3", 3)
    , ("4", 4)
    , ("5", 5)
    , ("6", 6)
    , ("7", 7)
    , ("8", 8)
    , ("9", 9)
    , ("zero", 0)
    , ("one", 1)
    , ("two", 2)
    , ("three", 3)
    , ("four", 4)
    , ("five", 5)
    , ("six", 6)
    , ("seven", 7)
    , ("eight", 8)
    , ("nine", 9)
    ]

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
