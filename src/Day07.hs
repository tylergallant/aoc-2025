module Day07 (day07) where

import qualified Data.Map as M (fromSet, lookup)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Set (Set, fromList, intersection, lookupMin, notMember)
import qualified Data.Set as S (filter)
import Utils (getInput)

type Pos = (Int, Int)
type Grid = [(Pos, Char)]
type Entrypoint = Pos
type Splitter = Pos
type Splitters = Set Splitter
type Manifold = (Entrypoint, Splitters)

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

isBelow :: Pos -> Pos -> Bool
isBelow (x1, y1) (x2, y2) = x1 == x2 && y1 < y2

solution2 :: Manifold -> Int
solution2 ((x, y), splitters) = fromMaybe 1 $ M.lookup firstSplitter timelines
  where
    firstSplitter = (x, y + 2)
    timelines = M.fromSet splitterTimelines splitters
    splitterTimelines = fromMaybe 1 . sumTimelines . reachableFrom
    reachableFrom splitter = catMaybes [left splitter, right splitter]
    left (x', y') = lookupMin $ S.filter (isBelow (x' - 1, y')) splitters
    right (x', y') = lookupMin $ S.filter (isBelow (x' + 1, y')) splitters
    sumTimelines [] = Just 2
    sumTimelines [x'] = (+1) <$> M.lookup x' timelines
    sumTimelines (x':y':_) = do
      x'' <- M.lookup x' timelines
      y'' <- M.lookup y' timelines
      Just $ x'' + y''

runSolutions :: Manifold -> IO ()
runSolutions manifold = do
  print $ solution1 manifold
  print $ solution2 manifold

day07 :: IO ()
day07 = do
  input <- getInput "day07-input.txt"
  maybe parseFailure runSolutions $ parseManifold input
    where parseFailure = putStrLn "Failed to parse input"
