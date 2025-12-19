module Day06 (day06) where

import Control.Applicative ((<|>), some, many)
import Data.Char (isDigit)
import Data.List (transpose)
import Text.ParserCombinators.ReadP (ReadP, char, munch1, sepBy)
import Utils (getInput, createSolver)

data Op = Add | Mul deriving Show
type Problem = ([Int], Op)

parser :: ReadP [Problem]
parser = zip <$> operands <*> operators
  where
    operands = fmap transpose . many $ operandLine <* newline
    operandLine = manySpaces *> sepBy operand someSpaces <* manySpaces
    operand = read <$> munch1 isDigit
    operators = manySpaces *> operatorLine <* manySpaces <* char '\n'
    operatorLine = sepBy operator someSpaces
    operator = add <|> mul
    add = Add <$ char '+'
    mul = Mul <$ char '*'
    someSpaces = some $ char ' '
    manySpaces = many $ char ' '
    newline = char '\n'

solveProblem :: Problem -> Int
solveProblem (operands, Add) = foldr (+) 0 operands
solveProblem (operands, Mul) = foldr (*) 1 operands

solution1 :: [Problem] -> Int
solution1 = sum . map solveProblem

day06 :: IO ()
day06 = do
  input <- getInput "day06-input.txt"
  let solve = createSolver parser input
  solve solution1
