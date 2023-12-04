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

type Input = [ScratchCard]
type Solution = Int

type Parser = Parsec Void T.Text

data ScratchCard = ScratchCard
  { card :: !Int
  , numbers :: !(Set.Set Int)
  , winningNumbers :: !(Set.Set Int)
  }
  deriving stock (Show)

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

part1 :: Input -> Solution
part1 = sum . fmap cardPoints
 where
  cardPoints :: ScratchCard -> Int
  cardPoints card =
    countPoints $ Set.size $ Set.intersection card.numbers card.winningNumbers
   where
    countPoints :: Int -> Int
    countPoints 0 = 0
    countPoints 1 = 1
    countPoints n = 2 * countPoints (n - 1)

part2 :: Input -> Solution
part2 = sum . Map.elems . foldl' scoreCard Map.empty
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
  [part, filepath] <- getArgs
  input <- parser filepath <$> B.readFile filepath
  if read @Int part == 1
    then do
      putStrLn "solution to problem 1 is:"
      print $ part1 input
    else do
      putStrLn "solution to problem 2 is:"
      print $ part2 input
