module Main (main) where

import Control.Applicative (asum)
import Data.ByteString qualified as B
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import System.Environment (getArgs)

type Input = [T.Text]
type Solution = Int

parser :: B.ByteString -> Input
parser = T.lines . decodeUtf8

part1 :: Input -> Solution
part1 = sum . fmap extractNumber
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

part2 :: Input -> Solution
part2 = sum . mapMaybe extractNumber
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
  [part, filepath] <- getArgs
  input <- parser <$> B.readFile filepath
  if read @Int part == 1
    then do
      putStrLn "solution to problem 1 is:"
      print $ part1 input
    else do
      putStrLn "solution to problem 2 is:"
      print $ part2 input
