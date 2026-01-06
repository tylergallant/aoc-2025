module Day09 where

import Data.Char (isDigit)
import Data.List (tails)
import Text.ParserCombinators.ReadP (ReadP, char, munch1, sepBy)
import Utils (createSolver, getInput)

type RedTile = (Int, Int)

parser :: ReadP [RedTile]
parser = sepBy tile newline <* newline
  where
    tile = (,) <$> numComma <*> num
    numComma = num <* char ','
    num = read <$> munch1 isDigit
    newline = char '\n'

pairs :: [a] -> [(a, a)]
pairs xs = [(x, y) | (x:ys) <- tails xs, y <- ys]

area :: (RedTile, RedTile) -> Int
area ((a, b), (x, y)) = width * height
  where
    width = abs (a - x) + 1
    height = abs (b - y) + 1

solution1 :: [RedTile] -> Int
solution1 = maximum . map area . pairs

day09 :: IO ()
day09 = do
  input <- getInput "day09-input.txt"
  let solve = createSolver parser input
  solve solution1
