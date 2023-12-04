module Main (main) where

import Data.ByteString qualified as B
import Data.Foldable (foldl')
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Void (Void)
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

{- Types for your input and your solution

- Input    should as the type of your input parameter. AOC, typically uses arrays, matrices or complex data structures.
- Solution should be the type of your solution. Typically is an Int, but It can be other things, like a list of numbers
         or a list of characters
-}
type Input = [ScratchCard] -- default to Bytestring, but very likely you'll need to change it
type Solution = Int

type Parser = Parsec Void T.Text

data ScratchCard = ScratchCard
  { card :: !Int
  , numbers :: !(Set.Set Int)
  , winningNumbers :: !(Set.Set Int)
  }
  deriving stock (Show)

-- | parser transforms a raw bytestring (from your ./input/day-X.input) to your Input type.
--   this is intended to use attoparsec for such a transformation. You can use Prelude's
--   String if it fit better for the problem
parser :: String -> B.ByteString -> Input
parser filepath = parseOrError scratchCards filepath . decodeUtf8
 where
  parseOrError :: Parser a -> String -> T.Text -> a
  parseOrError p path = either (error . errorBundlePretty) id . parse p path

  scratchCards :: Parser [ScratchCard]
  scratchCards = many scratchCard <* eof

  scratchCard :: Parser ScratchCard
  scratchCard = do
    string "Card" >> space
    card <- number
    char ':' >> space
    winningNumbers <- Set.fromList <$> many number
    char '|' >> space
    numbers <- Set.fromList <$> many number
    return $ ScratchCard {..}

  number :: Parser Int
  number = L.lexeme space L.decimal

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 = sum . fmap cardPoints
 where
  cardPoints :: ScratchCard -> Int
  cardPoints card =
    countPoints $ Set.size $ Set.intersection card.numbers card.winningNumbers
   where
    countPoints :: Int -> Int
    countPoints 0 = 0
    countPoints 1 = 1
    countPoints n = 2 * countPoints (n - 1)

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 = sum . Map.elems . foldl' scoreCard Map.empty
 where
  scoreCard :: Map.Map Int Int -> ScratchCard -> Map.Map Int Int
  scoreCard scores card =
    let
      copies = fromMaybe 0 $ Map.lookup card.card scores
      cardTotal = copies + 1
      newCopies = Map.fromList $ take (cardPoints card) $ fmap (,cardTotal) $ iterate (+ 1) (card.card + 1)
      cardScores = Map.unionWith (+) newCopies $ Map.fromList [(card.card, 1)]
     in
      Map.unionWith (+) scores cardScores

  cardPoints :: ScratchCard -> Int
  cardPoints card = Set.size $ Set.intersection card.numbers card.winningNumbers

main :: IO ()
main = do
  -- run this with cabal run -- day-x <part-number> <file-to-solution>
  -- example: cabal run -- day-3 2 "./input/day-3.example"
  -- will run part two of day three with input file ./input/day-3.example
  [part, filepath] <- getArgs
  input <- parser filepath <$> B.readFile filepath -- use parser <$> readFile filepath if String is better
  if read @Int part == 1
    then do
      putStrLn "solution to problem 1 is:"
      print $ solve1 input
    else do
      putStrLn "solution to problem 2 is:"
      print $ solve2 input
