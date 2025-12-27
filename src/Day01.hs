module Day01 (day01) where

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.List (mapAccumL, scanl')
import Prelude hiding (Left, Right)
import Text.ParserCombinators.ReadP (ReadP, char, many1, satisfy, sepBy)
import Utils (createSolver, getInput)

type Rotation = Int
type Dial = Int
type Password = Int

parser :: ReadP [Rotation]
parser = sepBy rotation newline <* newline
  where
    newline = char '\n'
    rotation = ($) <$> direction <*> distance
    distance = fmap read . many1 $ satisfy isDigit
    direction = left <|> right
    left = negate <$ char 'L'
    right = id <$ char 'R'

start :: Dial
start = 50

limitToDialSize :: Dial -> Dial
limitToDialSize = (`mod` 100)

visitedPositions :: Dial -> Rotation -> [Dial]
visitedPositions d r
  | r >= 0 = [d + 1 .. d + r]
  | otherwise = [d + r .. d - 1]

applyRotation :: Dial -> Rotation -> (Dial, [Dial])
applyRotation d r = (d + r, visitedPositions d r)

countHits :: [Dial] -> Password
countHits = length . filter (==0)

solution1 :: [Rotation] -> Password
solution1 = countHits . scanl' endPositions start
  where endPositions d r = limitToDialSize . fst $ applyRotation d r

solution2 :: [Rotation] -> Password
solution2 = countHits . visited
  where
    visited = map limitToDialSize . concat . snd . rotationsApplied
    rotationsApplied = mapAccumL applyRotation start

day01 :: IO ()
day01 = do
  input <- getInput "day01-input.txt"
  let solve = createSolver parser input
  solve solution1
  solve solution2
