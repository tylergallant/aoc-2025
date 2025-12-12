module Day01 (day01) where

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Prelude hiding (Left, Right)
import Text.ParserCombinators.ReadP (ReadP, char, many1, satisfy, sepBy)
import Utils (createSolver, getInput)

type Distance = Int
data Direction = Left | Right deriving Show
data Rotation = Rotation Direction Distance deriving Show
type DialPosition = Int
type Password = Int

dialSize :: Int
dialSize = 100

applyRotation :: DialPosition -> Rotation -> DialPosition
applyRotation p (Rotation Left l) = (p - l) `mod` dialSize
applyRotation p (Rotation Right r) = (p + r) `mod` dialSize

parser :: ReadP [Rotation]
parser = sepBy rotation sep <* char '\n'
  where
    sep = char '\n'
    rotation = Rotation <$> direction <*> distance
    distance = fmap read . many1 $ satisfy isDigit
    direction = left <|> right
    left = Left <$ char 'L'
    right = Right <$ char 'R'

solution1 :: [Rotation] -> Password
solution1 = length . filter (== 0) . scanl applyRotation startingPosition
  where startingPosition = 50

day01 :: IO ()
day01 = do
  input <- getInput "day01-input.txt"
  let solve = createSolver parser input
  solve solution1
