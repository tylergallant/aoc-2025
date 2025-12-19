module Day06 (day06) where

import Control.Applicative ((<|>), some, many)
import Data.Char (digitToInt, isDigit)
import Data.List (transpose)
import Text.ParserCombinators.ReadP (ReadP, char, satisfy, sepBy)
import Utils (getInput, createSolver)

data Op = Add | Mul
type Digit = Int
data GridItem = Digit Digit | Empty deriving Eq
type Grid = [[GridItem]]
type Input = (Grid, [Op])
type OperandParser = Grid -> [[Int]]
type Problem = ([Int], Op)

gridParser :: ReadP Grid
gridParser = many gridLine
  where
    gridLine = many gridItem <* newline
    gridItem = digit <|> empty
    digit = Digit . digitToInt <$> satisfy isDigit
    empty = Empty <$ char ' '
    newline = char '\n'

operatorsParser :: ReadP [Op]
operatorsParser = manySpaces *> operatorLine <* manySpaces <* char '\n'
  where
    operatorLine = sepBy operator someSpaces
    operator = add <|> mul
    add = Add <$ char '+'
    mul = Mul <$ char '*'
    someSpaces = some $ char ' '
    manySpaces = many $ char ' '

inputParser :: ReadP Input
inputParser = (,) <$> gridParser <*> operatorsParser

combineDigits :: [Digit] -> Int
combineDigits = foldl' addDigit 0
  where addDigit a b = a * 10 + b

gridItemsToOperand :: [GridItem] -> Int
gridItemsToOperand = combineDigits . foldr removeEmpties []
  where
    removeEmpties Empty xs = xs
    removeEmpties (Digit d) xs = d:xs

parseRow :: [GridItem] -> [Int]
parseRow = map gridItemsToOperand . foldr separateCols [] . dropWhile (== Empty)
  where
    separateCols Empty [] = []
    separateCols Empty ([]:ns) = []:ns
    separateCols Empty (n:ns) = []:n:ns
    separateCols d [] = [[d]]
    separateCols d ([]:ns) = [d]:ns
    separateCols d (n:ns) = (d:n):ns

parseOperands1 :: OperandParser
parseOperands1 = transpose . map parseRow

parseOperands2 :: OperandParser
parseOperands2 = foldr separateRows [] . map parseRow . transpose
  where
    separateRows [] [] = []
    separateRows [] ([]:ys) = []:ys
    separateRows [] (y:ys) = []:y:ys
    separateRows (x:_) [] = [[x]]
    separateRows (x:_) ([]:ys) = [x]:ys
    separateRows (x:_) (y:ys) = (x:y):ys

solveProblem :: Problem -> Int
solveProblem (operands, Add) = foldr (+) 0 operands
solveProblem (operands, Mul) = foldr (*) 1 operands

solution :: OperandParser -> Input -> Int
solution operands (grid', ops) = sum $ solveProblem <$> zip (operands grid') ops

day06 :: IO ()
day06 = do
  input <- getInput "day06-input.txt"
  let solve = createSolver inputParser input
  solve $ solution parseOperands1
  solve $ solution parseOperands2
