module Shared.Matrix (
    module Shared.Matrix,
    module Shared.Matrix.Types,
) where

import Control.Monad (forM_)
import Data.Array ((!))
import Data.Array.Base qualified as Arr
import Shared.Matrix.Types
import System.IO (readFile')

readAsChars :: String -> IO (Matrix Char)
readAsChars fileName = do
    fileContents <- lines <$> readFile' fileName
    let linesArray = Arr.listArray (0, length fileContents - 1) fileContents
    pure $ fmap (\line -> Arr.listArray (0, length line - 1) line) linesArray

-- note: left is y, right is x
iterM_ :: (Monad m) => Matrix a -> (MatrixIndex -> m ()) -> m ()
iterM_ matrix fn =
    forM_ [0 .. length matrix - 1] $ \i ->
        forM_ [0 .. length (matrix ! 0) - 1] $ \j ->
            fn (j, i)

getX :: Matrix a -> Int
getX = length

getY :: Matrix a -> Int
getY = length . (! 0)

getYandX :: Matrix a -> (Int, Int)
getYandX matrix = (getY matrix, getX matrix)

make :: (Int, Int) -> ((Int, Int) -> a) -> Matrix a
make (y, x) generator = Arr.genArray (0, y - 1) $
    \j -> Arr.genArray (0, x - 1) $
        \i -> generator (j, i)
