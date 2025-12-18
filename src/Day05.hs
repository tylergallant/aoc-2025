module Day05 (day05) where

import Data.Char (isDigit)
import Data.List (sort)
import Text.ParserCombinators.ReadP (ReadP, char, munch1, sepBy, string)
import Utils (getInput, createSolver)

type Ingredient = Int
type FreshnessRange = (Ingredient, Ingredient)
type IngredientList = [Ingredient]

rangeContains :: Ingredient -> FreshnessRange -> Bool
rangeContains id' (lower, upper) = lower <= id' && id' <= upper

isFresh :: [FreshnessRange] -> Ingredient -> Bool
isFresh ranges = (`any` ranges) . rangeContains

rangeSize :: FreshnessRange -> Int
rangeSize (lower, upper) = upper - lower + 1

rangesOverlap :: FreshnessRange -> FreshnessRange -> Bool
rangesOverlap (l1, u1) (l2, u2) = l1 <= u2 && l2 <= u1

combineRanges :: FreshnessRange -> FreshnessRange -> FreshnessRange
combineRanges (l1, u1) (l2, u2) = (min l1 l2, max u1 u2)

mergeRanges :: [FreshnessRange] -> [FreshnessRange]
mergeRanges [] = []
mergeRanges [r] = [r]
mergeRanges (r1:r2:rs)
  | rangesOverlap r1 r2 = mergeRanges $ combineRanges r1 r2 : rs
  | otherwise = r1 : mergeRanges (r2 : rs)

parser :: ReadP ([FreshnessRange], IngredientList)
parser = (,) <$> ranges <*> ingredients
  where
    ranges = sepBy range lineSep <* string "\n\n"
    range = (,) <$> lower <*> upper
    lower = read <$> munch1 isDigit <* char '-'
    upper = read <$> munch1 isDigit
    ingredients = sepBy ingredient lineSep <* lineSep
    ingredient = read <$> munch1 isDigit
    lineSep = char '\n'

solution1 :: ([FreshnessRange], IngredientList) -> Int
solution1 (ranges, ingredients) = length $ onlyFresh ingredients
  where onlyFresh = filter $ isFresh ranges

solution2 :: ([FreshnessRange], IngredientList) -> Int
solution2 (ranges, _) = sum . map rangeSize . mergeRanges $ sort ranges

day05 :: IO ()
day05 = do
  input <- getInput "day05-input.txt"
  let solve = createSolver parser input
  solve solution1
  solve solution2
