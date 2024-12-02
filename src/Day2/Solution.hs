module Day2.Solution where

import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as Text
import System.IO (readFile')

fileName :: String
fileName = "src/Day2/input.txt"

input :: IO [Text]
input = fmap Text.pack . lines <$> readFile' fileName

type Line = [Integer]

inputNums :: IO [Line]
inputNums = fmap (fmap (fmap (read @Integer . Text.unpack) . Text.splitOn delimiter)) input
  where
    delimiter = " "

validateLine_1 :: Line -> Bool
validateLine_1 = \case
    [_] -> True
    [a, b] -> predicate a b
    (a : b : ct) | predicate a b -> validateLine_1 (b : ct)
    _ -> False
  where
    predicate a b = let val = abs (a - b) in (val >= 1) && (val <= 3)

validateLine_2 :: Line -> Bool
validateLine_2 xs = let sorted = sort xs in sorted == xs || reverse sorted == xs

validateLine :: Line -> Bool
validateLine xs = validateLine_1 xs && validateLine_2 xs

howManyAreValid :: [Line] -> Int
howManyAreValid = length . filter validateLine

solution :: IO Int
solution = fmap howManyAreValid inputNums

-- >>> solution
-- 591

-- Every permutation of `xs` from which a single element is removed
versions :: [a] -> [[a]]
versions xs =
    let fullPairs = zip [0 ..] $ replicate (length xs) xs
        removeAtIdx idx elts = take idx elts ++ drop (idx + 1) elts
        exclude = uncurry removeAtIdx
        excluded = fmap exclude fullPairs
     in excluded

validateLine' :: Line -> Bool
validateLine' xs = validateLine xs || cond
  where
    cond = (/= 0) $ length $ filter id $ fmap validateLine (versions xs)

howManyAreValid' :: [Line] -> Int
howManyAreValid' = length . filter validateLine'

solution' :: IO Int
solution' = fmap howManyAreValid' inputNums

-- >>> solution'
-- 621
