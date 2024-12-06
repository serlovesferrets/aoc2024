module Shared.Matrix.Operators where

import Data.Array ((!))
import Data.Array.Base ((!?))
import Shared.Matrix.Types (MatrixIndex, Matrix)

(!!!) :: Matrix a -> MatrixIndex -> a
matrix !!! (y, x) = (matrix ! y) ! x

(!!?) :: Matrix a -> MatrixIndex -> Maybe a
matrix !!? (y, x) = (matrix !? y) >>= (!? x)

(?!!) :: Matrix a -> MatrixIndex -> Maybe a
matrix ?!! (x, y) = (matrix !? y) >>= (!? x)
