module Day02 where

import Data.Char (isDigit)
import Text.ParserCombinators.ReadP (ReadP, char, munch1, sepBy)
import Utils (createSolver, getInput)

type Id = String
type Range = (Id, Id)

idInvalid :: Id -> Bool
idInvalid i = a == b
  where
    (a, b) = splitHalf i
    splitHalf xs = let n = length xs `div` 2 in splitAt n xs

expandRange :: Range -> [Id]
expandRange (a, b) = show <$> ids
  where ids = [read a .. read b] :: [Int]

parser :: ReadP [Range]
parser = sepBy parseRange sep <* char '\n'
  where
    sep = char ','
    parseId = munch1 isDigit
    parseRange = (\a _ b -> (a, b)) <$> parseId <*> char '-' <*> parseId

solution1 :: [Range] -> Int
solution1 ranges = sum . map read . filter idInvalid $ ranges >>= expandRange

day02 :: IO ()
day02 = do
  input <- getInput "day02-input.txt"
  let solve = createSolver parser input
  solve solution1
