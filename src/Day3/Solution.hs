module Day3.Solution where

import Control.Monad (void)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import System.IO (readFile')
import Text.Megaparsec
import Text.Megaparsec.Char

fileName :: String
fileName = "src/Day3/input.txt"

input :: IO Text
input = Text.strip . Text.pack <$> readFile' fileName

type Parser = Parsec Void Text

data Instruction where
    Mul :: Integer -> Integer -> Instruction
    deriving (Show)

evaluate :: Instruction -> Integer
evaluate = \case
    Mul a b -> a * b

mulInstruction :: Parser Instruction
mulInstruction = do
    _ <- string "mul("
    left <- some digitChar
    _ <- char ','
    right <- some digitChar
    _ <- char ')'
    pure $ Mul (read @Integer left) (read @Integer right)

noise :: Parser Char
noise = printChar <|> char '\n'

untilMul :: Parser Instruction
untilMul = noise `skipManyTill` try mulInstruction

parser :: Parser [Instruction]
parser = untilMul `manyTill` notFollowedBy untilMul

solution :: IO Integer
solution = do
    inputText <- input
    let parsed = parse parser "" inputText
    let instructions = case parsed of
            Left _ -> undefined
            Right ok -> ok
    print instructions
    pure $ sum $ fmap evaluate instructions

-- >>> solution
-- 165225049

parseDont :: Parser ()
parseDont = void $ string "don't()"

parseDo :: Parser ()
parseDo = void $ string "do()"

data Ignored = Ignored

ignored :: Parser Ignored
ignored = (parseDont *> noise `manyTill` parseDo) $> Ignored
