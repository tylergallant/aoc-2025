module Day02 (day02) where

import Data.Char (isDigit)
import Data.List (inits, stripPrefix)
import Text.ParserCombinators.ReadP (ReadP, char, munch1, sepBy)
import Utils (createSolver, getInput)

type Id = String
type IdSegment = String
type Range = (Id, Id)

solution1 :: Id -> Bool
solution1 i = a == b
  where
    (a, b) = splitHalf i
    splitHalf xs = let n = length xs `div` 2 in splitAt n xs

checkCandidatePattern :: IdSegment -> Id -> Bool
checkCandidatePattern _ [] = True
checkCandidatePattern pattern i = maybe False checkNext $ stripPrefix pattern i
  where checkNext = checkCandidatePattern pattern

candidatePatterns :: Id -> [IdSegment]
candidatePatterns i = take upToHalfDigits . drop 1 $ inits i
  where upToHalfDigits = length i `div` 2

solution2 :: Id -> Bool
solution2 i = any (flip checkCandidatePattern i) $ candidatePatterns i

expandRange :: Range -> [Id]
expandRange (a, b) = show <$> ids
  where ids = [read a .. read b] :: [Int]

parser :: ReadP [Range]
parser = sepBy parseRange sep <* char '\n'
  where
    sep = char ','
    parseId = munch1 isDigit
    parseRange = (\a _ b -> (a, b)) <$> parseId <*> char '-' <*> parseId

makeSolution :: (Id -> Bool) -> [Range] -> Int
makeSolution solution ranges = sum . map read $ filter solution ids
  where ids = ranges >>= expandRange

day02 :: IO ()
day02 = do
  input <- getInput "day02-input.txt"
  let solve = createSolver parser input
  solve $ makeSolution solution1
  solve $ makeSolution solution2
