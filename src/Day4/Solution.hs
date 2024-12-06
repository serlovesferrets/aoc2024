module Day4.Solution where

import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Control.Monad.State (MonadIO (liftIO), StateT, execStateT, modify)
import Data.Array.Base ((!))
import Data.Maybe (catMaybes, isJust)
import GHC.Base (when)
import Shared.Matrix (Matrix)
import qualified Shared.Matrix as Matrix
import Shared.Matrix.Operators

fileName :: String
fileName = "src/Day4/input.txt"

input :: IO (Matrix Char)
input = Matrix.readAsChars fileName

type Position = (Int, Int)

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

check' :: Matrix Char -> Position -> Maybe ()
check' matr start@(x, y) = do
    'A' <- matr !!? start
    conf_1 <|> conf_2 <|> conf_3 <|> conf_4
  where
    topRight = (x + 1, y + 1)
    topLeft = (x - 1, y + 1)
    btmLeft = (x - 1, y - 1)
    btmRight = (x + 1, y - 1)

    conf_1 = do
        'M' <- matr !!? topLeft
        'S' <- matr !!? btmRight
        'M' <- matr !!? btmLeft
        'S' <- matr !!? topRight
        pure ()

    conf_2 = do
        'M' <- matr !!? topLeft
        'S' <- matr !!? btmLeft
        'M' <- matr !!? topRight
        'S' <- matr !!? btmRight
        pure ()

    conf_3 = do
        'M' <- matr !!? topRight
        'S' <- matr !!? topLeft
        'M' <- matr !!? btmRight
        'S' <- matr !!? btmLeft
        pure ()

    conf_4 = do
        'M' <- matr !!? btmLeft
        'S' <- matr !!? topLeft
        'M' <- matr !!? btmRight
        'S' <- matr !!? topRight
        pure ()

solutionM' :: Stateful Int ()
solutionM' = do
    inputElts <- liftIO input
    forM_ [0 .. length inputElts - 1] $ \i ->
        forM_ [0 .. length (inputElts ! i) - 1] $ \j ->
            when (isJust (check' inputElts (j, i))) $ modify (+ 1)

solution' :: IO Int
solution' = execStateT solutionM' 0

-- >>> solution'
-- 1873
