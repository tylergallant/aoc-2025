module Day04 (day04) where

import Data.Set (Set, fromList, member)
import Utils (getInput)

type Position = (Int, Int)
type UnindexedDiagram = String
type IndexedDiagram = [(Position, Char)]
type RollPositions = Set Position

indexDiagram :: UnindexedDiagram -> IndexedDiagram
indexDiagram = concat . zipWith g [0..] . lines
  where
    g y = zipWith (h y) [0..]
    h y x c = ((x, y), c)

onlyRolls :: IndexedDiagram -> IndexedDiagram
onlyRolls = filter isRoll
  where isRoll ((_,_), c) = c == '@'

onlyPositions :: IndexedDiagram -> RollPositions
onlyPositions = fromList . map fst

neighbors :: Position -> [Position]
neighbors (x, y) =
  [ (x', y')
  | x' <- [x-1..x+1]
  , y' <- [y-1..y+1]
  , (x', y') /= (x, y)
  ]

rollNeighbors :: Position -> RollPositions -> Int
rollNeighbors roll rolls = length . filter isRoll $ neighbors roll
  where isRoll = flip member rolls

parser :: UnindexedDiagram -> RollPositions
parser = onlyPositions . onlyRolls . indexDiagram

solution1 :: RollPositions -> Int
solution1 rolls = length . onlyAccessible $ foldr rollAccessibility [] rolls
  where
    rollAccessibility roll results = rollNeighbors roll rolls : results
    onlyAccessible = filter (< accessibilityThreshold)
    accessibilityThreshold = 4

day04 :: IO ()
day04 = do
  input <- getInput "day04-input.txt"
  print . solution1 $ parser input
