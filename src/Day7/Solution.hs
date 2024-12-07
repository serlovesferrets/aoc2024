module Day7.Solution where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Either (fromRight)
import Data.Maybe (isJust)
import Data.Text qualified as Text
import Data.Text.Read (decimal)
import System.IO (readFile')

filePath :: String
filePath = "src/Day7/input.txt"

unsafeListToTuple :: [a] -> (a, a)
unsafeListToTuple [a, b] = (a, b)
unsafeListToTuple _ = error "INVALID INPUT TO SPLIT!"

unwrapRight :: Either a (c, b) -> c
unwrapRight = fst . fromRight undefined

input :: IO [(Integer, [Integer])]
input = do
    contents <- readFile' filePath
    let rawEquations = unsafeListToTuple . Text.splitOn ":" . Text.pack <$> lines contents
        splitOnSpace = Text.splitOn " "
        equations = do
            (leftRaw, rightRaw) <- rawEquations
            let left = unwrapRight $ decimal @Integer leftRaw
                right = unwrapRight . decimal @Integer <$> tail (splitOnSpace rightRaw)
            pure (left, reverse right)
    pure equations

data Operator = Plus | Times deriving (Show)

-- Always adds a "Plus" at the start, requires the list to be reversed
solve :: Integer -> [Integer] -> Maybe [Operator]
solve = \cases
    _ [] -> pure []
    n [x] -> plus n x <|> times n x
    n (x : xt) ->
        (Plus :) <$> solve (n - x) xt
            <|> (Times :) <$> (if n `mod` x == 0 then solve (n `div` x) xt else Nothing)
  where
    plus n a = if n - a == 0 then pure [Plus] else Nothing
    times n a = if n `mod` a == 0 && n `div` a == 1 then pure [Times] else Nothing

solution :: IO Integer
solution = do
    equations <- input
    pure . sum $ do
        (testValue, nums) <- equations
        guard (isJust $ solve testValue nums)
        pure testValue

-- >>> solution
-- 2437272016585
