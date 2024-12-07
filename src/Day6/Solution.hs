module Day6.Solution where

import Control.Monad.State.Strict (MonadState (get, put), MonadTrans (lift), State, StateT, execState, execStateT, modify)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Shared.Matrix qualified as Matrix
import Shared.Matrix.Operators
import Shared.Matrix.Types (Matrix)

fileName :: String
fileName = "src/Day6/input.txt"

input :: IO (Matrix Char)
input = Matrix.readAsChars fileName

type Position = (Int, Int)
data Direction = Up | Dn | Lt | Rt deriving (Show, Eq, Ord)

parseDirection :: Char -> Maybe Direction
parseDirection = \case
    '^' -> pure Up
    'v' -> pure Dn
    '<' -> pure Lt
    '>' -> pure Rt
    _ -> Nothing

findGuardPosition :: Matrix Char -> (Position, Direction)
findGuardPosition matrix = execState go undefined
  where
    go = Matrix.iterM_ matrix $ \pos ->
        case parseDirection (matrix !!! pos) of
            Just dr -> put (pos, dr)
            Nothing -> pure ()

guardPosition :: IO (Position, Direction)
guardPosition = findGuardPosition <$> input

turnSquare :: Direction -> Direction
turnSquare = \case
    Up -> Rt
    Rt -> Dn
    Dn -> Lt
    Lt -> Up

move :: Position -> Direction -> Position
move (y, x) = \case
    Up -> (y - 1, x)
    Dn -> (y + 1, x)
    Lt -> (y, x - 1)
    Rt -> (y, x + 1)

data GuardState = GuardState
    { _position :: Position
    , _direction :: Direction
    , _counter :: Int
    }
    deriving (Show)

moveGuard :: GuardState -> GuardState
moveGuard (GuardState pos dir counter) = GuardState (move pos dir) dir (counter + 1)

turnGuard :: GuardState -> GuardState
turnGuard (GuardState pos dir counter) = GuardState pos (turnSquare dir) counter

getNextPosition :: GuardState -> Position
getNextPosition = _position . moveGuard

data Next = Empty | Obstacle | Outside deriving (Show)

parseChar :: Char -> Next
parseChar = \case
    '#' -> Obstacle
    _ -> Empty

peekNext :: Matrix Char -> GuardState -> Next
peekNext matrix gs = fromMaybe Outside $ do
    ch <- matrix !!? _position (moveGuard gs)
    pure $ if ch == '#' then Obstacle else Empty

type VisitedPositions = Set Position
type RoamState a = StateT GuardState (State VisitedPositions) a

loop :: Matrix Char -> GuardState -> Int
loop matrix gs = Set.size $ execState (execStateT go gs) Set.empty
  where
    go :: RoamState ()
    go = do
        guard <- get @GuardState
        let nextPosition = getNextPosition guard
        case peekNext matrix guard of
            Outside -> pure ()
            Empty -> do
                modify moveGuard
                lift $ modify $ Set.insert nextPosition
                go
            Obstacle -> do modify turnGuard; go

rawGuard :: IO GuardState
rawGuard = do
    (pos, dir) <- guardPosition
    pure (GuardState pos dir 1)

solution :: IO Int
solution = do
    (pos, dir) <- guardPosition
    board <- input
    pure $ loop board (GuardState pos dir 0)

-- >>> solution
-- 5067
