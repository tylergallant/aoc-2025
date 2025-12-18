module Day05 (day05) where

import Data.Char (isDigit)
import Text.ParserCombinators.ReadP (ReadP, char, munch1, sepBy, string)
import Utils (getInput, createSolver)

type Ingredient = Int
type FreshnessRange = (Ingredient, Ingredient)
type IngredientList = [Ingredient]

rangeContains :: Ingredient -> FreshnessRange -> Bool
rangeContains id' (lower, upper) = lower <= id' && id' <= upper

isFresh :: [FreshnessRange] -> Ingredient -> Bool
isFresh ranges = (`any` ranges) . rangeContains

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

day05 :: IO ()
day05 = do
  input <- getInput "day05-input.txt"
  let solve = createSolver parser input
  solve solution1
