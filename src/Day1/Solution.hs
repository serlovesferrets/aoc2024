module Day1.Solution where

import Data.List (nub, sort)
import Data.Map.Lazy ((!?))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text (pack, splitOn, unpack)
import Optics.Core (Field1 (_1), at')
import Optics.Operators ((%~), (.~))
import Optics.Optic ((&))
import System.IO (readFile')

fileName :: String
fileName = "src/Day1/input.txt"

input :: IO [Text]
input = fmap Text.pack . lines <$> readFile' fileName

asPair :: Text -> (Integer, Integer)
asPair line =
    let separator = "   "
        list = Text.splitOn separator line
        pair = (head list, list !! 1)
     in mapTuple textToInteger pair
  where
    mapTuple f (left, right) = (f left, f right)
    textToInteger = read @Integer . Text.unpack

difference :: (Num a) => (a, a) -> a
difference (left, right) = abs (left - right)

inputAsNumbers :: IO [(Integer, Integer)]
inputAsNumbers = do
    contents <- input
    let pairs = asPair <$> contents
    pure pairs

sortedInput :: IO [(Integer, Integer)]
sortedInput = do
    contents <- inputAsNumbers
    let (left, right) = unzip contents
    pure $ zip (sort left) (sort right)

differences :: IO [Integer]
differences = do
    numbers <- sortedInput
    pure $ abs . difference <$> numbers

solution :: IO Integer
solution = sum <$> differences

-- >>> solution
-- 2815556

type Occurrences = Map Integer Integer

-- Note: both lists must be sorted, first list should be nubbed
findOccurrences :: Occurrences -> [Integer] -> [Integer] -> Occurrences
findOccurrences occ = \cases
    xs@(x : _) (y : yt) | x == y -> findOccurrences (incrementKey x occ) xs yt
    (x : xt) ys@(y : _) | x < y -> findOccurrences occ xt ys
    xs@(x : _) (y : yt) | x > y -> findOccurrences occ xs yt
    (_ : xt) ys -> findOccurrences occ xt ys
    _ [] -> occ
    [] _ -> occ
  where
    incrementOrPut = \case
        Nothing -> Just (1 :: Integer)
        Just n -> Just $ n + 1
    incrementKey key pairs = pairs & at' key .~ incrementOrPut (pairs !? key)

inputOccurrences :: IO Occurrences
inputOccurrences = do
    (left, right) <- (_1 %~ nub) . unzip <$> sortedInput
    pure $ findOccurrences Map.empty left right

solution' :: IO Integer
solution' = do
    occurrences <- inputOccurrences
    let asSimilarityScores = uncurry (*) <$> Map.toList occurrences
    pure $ sum asSimilarityScores

-- >>> solution'
-- 23927637
