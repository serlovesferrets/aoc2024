module Shared.Matrix.Types (MatrixIndex, Array, Matrix) where

import Data.Array qualified as Arr

type MatrixIndex = (Int, Int)

type Array = Arr.Array Int
type Matrix a = Array (Array a)
