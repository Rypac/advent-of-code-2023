module Main (main) where

import Data.ByteString qualified as B
import System.Environment (getArgs)

type Input = B.ByteString
type Solution = Int

parser :: B.ByteString -> Input
parser = undefined

part1 :: Input -> Solution
part1 = error "Part 1 not implemented"

part2 :: Input -> Solution
part2 = error "Part 2 not implemented"

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
