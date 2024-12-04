module Day4.Solution where

import Control.Monad (forM_)
import Control.Monad.State (MonadIO (liftIO), StateT, modify, execStateT)
import Data.Array qualified as Arr
import Data.Array.Base ((!), (!?))
import Data.Maybe (catMaybes)
import System.IO (readFile')

fileName :: String
fileName = "src/Day4/input.txt"

type Array = Arr.Array Int
type Matrix a = Array (Array a)

input :: IO (Matrix Char)
input = do
    fileContents <- lines <$> readFile' fileName
    let linesArray = Arr.listArray (0, length fileContents - 1) fileContents
    pure $ fmap (\line -> Arr.listArray (0, length line - 1) line) linesArray

-- note: left is y, right is x
type Position = (Int, Int)
type WithPos a = (Int, a)

(!!?) :: Matrix a -> Position -> Maybe a
matrix !!? (y, x) = (matrix !? y) >>= (!? x)

check :: Matrix Char -> Position -> (Position -> Position) -> Maybe ()
check matr start move = do
    'X' <- matr !!? start
    'M' <- matr !!? move start
    'A' <- matr !!? (move . move) start
    'S' <- matr !!? (move . move . move) start
    pure ()

checkPositions :: Matrix Char -> Position -> [Maybe ()]
checkPositions matr start =
    map
        (check matr start)
        [ \(y, x) -> (y, x + 1)
        , \(y, x) -> (y + 1, x + 1)
        , \(y, x) -> (y + 1, x)
        , \(y, x) -> (y + 1, x - 1)
        , \(y, x) -> (y, x - 1)
        , \(y, x) -> (y - 1, x - 1)
        , \(y, x) -> (y - 1, x)
        , \(y, x) -> (y - 1, x + 1)
        ]

countXmas :: Matrix Char -> Position -> Int
countXmas matr start = length . catMaybes $ checkPositions matr start

-- arrIdxMap :: i -> (i -> a -> b) -> Arr.Array i a -> Arr.Array i b
-- arrIdxMap acc fn arr = foldr

type Stateful s a = StateT s IO a

solutionM :: Stateful Int ()
solutionM = do
    inputElts <- liftIO input
    forM_ [0 .. length inputElts - 1] $ \i ->
        forM_ [0 .. length (inputElts ! i) - 1] $ \j ->
            let xmasCount = countXmas inputElts (i, j)
             in modify (+ xmasCount)

solution :: IO Int
solution = execStateT solutionM 0

-- >>> solution
-- 2524
