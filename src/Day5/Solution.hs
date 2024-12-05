module Day5.Solution where

import Control.Monad (forM_, guard)
import Control.Monad.State.Strict (MonadState (get), evalState, execState, modify)
import Data.IntMap ((!?))
import Data.IntMap qualified as Map
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet, (\\))
import Data.IntSet qualified as Set
import Data.List (elemIndex, sortBy)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import System.IO (readFile')

filePath :: String
filePath = "src/Day5/input.txt"

input :: IO ([Text], [Text])
input = do
    contents <- Text.pack <$> readFile' filePath
    let contentsLines = Text.splitOn "\n" contents
        indexOfEmpty = fromJust $ elemIndex "" contentsLines
        parts = splitAt indexOfEmpty contentsLines
     in pure parts

rawRules :: IO [Text]
rawRules = fst <$> input

rawPages :: IO [Text]
rawPages = removeEmptySpaces . tail . snd <$> input
  where
    removeEmptySpaces xs = take (length xs - 1) xs

type Rules = IntMap IntSet

parsePrecedence :: Text -> (Int, Int)
parsePrecedence str = both (read @Int . Text.unpack) (left, right)
  where
    split rule =
        let parts = Text.splitOn "|" rule
         in (head parts, head $ tail parts)
    (left, right) = split str
    both fn (a, b) = (fn a, fn b)

parsePrecedences :: [Text] -> Rules
parsePrecedences parts =
    let pairs = fmap parsePrecedence parts
        updates = execState $ do
            forM_ pairs $ \(k, v) ->
                modify $ Map.alter (update v) k
     in updates Map.empty
  where
    update :: Int -> Maybe IntSet -> Maybe IntSet
    update v = \case
        Nothing -> pure $ Set.singleton v
        Just set -> pure $ Set.insert v set

rules :: IO Rules
rules = parsePrecedences <$> rawRules

type Print = [Int]
type Pages = [Print]

pages :: IO Pages
pages = do
    rawPagesStrs <- rawPages
    let numListsStrs = Text.splitOn "," <$> rawPagesStrs
        numPages = do
            numList <- numListsStrs
            pure $ read @Int . Text.unpack <$> numList
    pure numPages

isPrintValid :: Rules -> Print -> Bool
isPrintValid _ [] = True
isPrintValid _ [_] = True
isPrintValid rs pr = evalState (result pr) Set.empty
  where
    result = \case
        [] -> pure True
        (p : pl) -> do
            inserted <- get
            let notAllowed = fromMaybe Set.empty (Map.lookup p rs)
                valid = notAllowed \\ inserted == notAllowed
            modify $ Set.insert p
            if valid
                then result pl
                else pure False

validPrints :: IO Pages
validPrints = do
    rs <- rules
    filter (isPrintValid rs) <$> pages

getMiddleElt :: [a] -> a
getMiddleElt xs = xs !! (length xs `div` 2)

solution :: IO Int
solution = sum . fmap getMiddleElt <$> validPrints

-- >>> solution
-- 5248

invalidPrints :: IO Pages
invalidPrints = do
    rs <- rules
    filter (not . isPrintValid rs) <$> pages

sortPrints :: Rules -> IO Pages
sortPrints rs = fmap (sortBy $ flip sorter) <$> invalidPrints
  where
    sorter :: Int -> Int -> Ordering
    sorter l r = fromMaybe EQ $ do
        greater <- rs !? l
        guard (r `Set.member` greater)
        pure GT

sortedPrints :: IO Pages
sortedPrints = rules >>= sortPrints

solution' :: IO Int
solution' = sum . fmap getMiddleElt <$> sortedPrints

-- >>> solution'
-- 4507
