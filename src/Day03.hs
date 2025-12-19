module Day03 (day03) where

import Data.Char (isDigit)
import Text.ParserCombinators.ReadP (ReadP, char, many1, satisfy, sepBy)
import Utils (createSolver, getInput)

type Battery = Int
type Bank = [Battery]
type Joltage = Int

parser :: ReadP [Bank]
parser = sepBy pBank sep <* char '\n'
  where
    sep = char '\n'
    pBattery = read . pure <$> satisfy isDigit
    pBank = many1 pBattery

-- Took inspiration from https://github.com/glguy/advent/blob/main/solutions/src/2025/03.hs
addDigit :: [Joltage] -> Battery -> [Joltage]
addDigit prev d = do
  (a, b) <- zip prev $ 0 : prev
  return . max a $ b * 10 + d

solveLine :: Bank -> [Joltage]
solveLine = foldl addDigit $ repeat 0

solution :: Int -> [Bank] -> Joltage
solution digits banks = sum $ do
  joltages <- solveLine <$> banks
  return $ joltages !! digits

solution1 :: [Bank] -> Joltage
solution1 = solution 1

solution2 :: [Bank] -> Joltage
solution2 = solution 11

day03 :: IO ()
day03 = do
  input <- getInput "day03-input.txt"
  let solve = createSolver parser input
  solve solution1
  solve solution2
