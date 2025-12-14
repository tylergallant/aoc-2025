module Day03 where

import Data.Char (isDigit)
import Text.ParserCombinators.ReadP (ReadP, char, many1, satisfy, sepBy)
import Utils (createSolver, getInput)

type Battery = Int
type Bank = [Battery]

parser :: ReadP [Bank]
parser = sepBy pBank sep <* char '\n'
  where
    sep = char '\n'
    pBattery = read . pure <$> satisfy isDigit
    pBank = many1 pBattery

maxJoltage :: Bank -> Int
maxJoltage = scan 0 0
  where
    scan tens ones [] = tens * 10 + ones
    scan tens ones [battery]
      | battery > ones = tens * 10 + battery
      | otherwise = tens * 10 + ones
    scan tens ones (battery : rest)
      | battery > tens = scan battery 0 rest
      | battery > ones = scan tens battery rest
      | otherwise = scan tens ones rest

solution1 :: [Bank] -> Int
solution1 = sum . map maxJoltage

day03 :: IO ()
day03 = do
  input <- getInput "day03-input.txt"
  let solve = createSolver parser input
  solve solution1
