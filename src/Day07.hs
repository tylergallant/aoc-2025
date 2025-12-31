module Day07 (day07) where

import Data.Maybe (listToMaybe)
import Data.Set (Set, fromList, intersection, notMember)
import qualified Data.Set as S (filter)
import Utils (getInput)

type Pos = (Int, Int)
type Entrypoint = Pos
type Splitter = Pos
type Splitters = Set Splitter
type Manifold = (Entrypoint, Splitters)
type Grid = [(Pos, Char)]

parseGrid :: String -> Grid
parseGrid s = do
  (y, l) <- zip [0..] $ lines s
  (x, c) <- zip [0..] l
  pure ((x, y), c)

positionsOf :: Grid -> Char -> [Pos]
positionsOf grid c = fmap fst . flip filter grid $ (==c) . snd

parseManifold :: String -> Maybe Manifold
parseManifold input = do
  entrypoint <- listToMaybe $ positionsOf grid 'S'
  let splitters = fromList $ positionsOf grid '^'
  pure (entrypoint, splitters)
    where grid = parseGrid input

isBeam :: Manifold -> Pos -> Bool
isBeam m@(e, s) p@(x, y) = y >= 0 && notMember p s && or
    [ p == e
    , isBeam m above
    , any (reachable m) $ intersection s beside
    ]
      where
        above = (x, y - 1)
        beside = fromList [(x - 1, y), (x + 1, y)]

reachable :: Manifold -> Splitter -> Bool
reachable manifold (x, y) = isBeam manifold (x, y - 1)

solution1 :: Manifold -> Int
solution1 manifold@(_, splitters) = length $ S.filter isReachable splitters
  where isReachable = reachable manifold

day07 :: IO ()
day07 = do
  input <- getInput "day07-input.txt"
  let manifold = parseManifold input
  print $ solution1 <$> manifold
