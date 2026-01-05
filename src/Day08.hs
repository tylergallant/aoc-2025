module Day08 (day08) where

import Control.Monad (replicateM_, unless)
import Control.Monad.State (StateT, evalStateT, execStateT, get, gets, modify)
import Control.Monad.Trans (lift)
import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.Foldable (find)
import Data.List (sortOn, tails, uncons)
import Data.Maybe (fromMaybe)
import Data.Ord (Down(..))
import Data.Set (Set, (\\), fromList, insert, member, singleton, toList)
import qualified Data.Set as S (map)
import Text.ParserCombinators.ReadP (ReadP, char, munch1, sepBy)
import Utils (createSolver, getInput)

type Box = (Int, Int, Int)
type Boxes = Set Box
type Connection = (Box, Box)
type Connections = [Connection]
type Circuit = Boxes
type Circuits = Set Circuit

-- Read in a set of boxes where each box is a singleton circuit
parser :: ReadP Boxes
parser = fromList <$> sepBy box newline <* newline
  where
    box = (,,) <$> coordComma <*> coordComma <*> coord
    coordComma = coord <* char ','
    coord = read <$> munch1 isDigit
    newline = char '\n'

-- Distance a connection must span
distance :: Connection -> Double
distance ((p1, p2, p3), (q1, q2, q3)) = sqrt $ fromIntegral diffs
  where
    diffs = sum $ square <$> zipWith (-) [p1, p2, p3] [q1, q2, q3]
    square = (^(2 :: Int))

-- All possible connections between boxes, sorted by distance
connections :: Boxes -> [Connection]
connections = sortOn distance . pairs . toList
  where pairs items = [(x, y) | x:xs <- tails $ items, y <- xs]

{- connect two boxes if they're not already part of the same circuit to form a
   new circuit -}
connect
  :: Box
  -> Box
  -> Circuit
  -> Circuit
  -> StateT (Circuits, Connections) Maybe Connection
connect x y a b = do
  unless (x `member` b) $ do
    circuits <- gets fst
    let newCircuits = insert (a <> b) $ circuits \\ fromList [a, b]
    modify . first $ const newCircuits
  pure (x, y)

-- Find the nearest two boxes to connect and then try to connect them
connectNearest :: StateT (Circuits, Connections) Maybe Connection
connectNearest = do
  (circuits, conns) <- get
  ((x, y), conns') <- lift $ uncons conns
  modify $ (conns' <$)
  let findCircuit = flip find circuits . member
  fromMaybe connectNearest $ connect x y <$> findCircuit x <*> findCircuit y

{- make the 1000 shortest connections then multiply the sizes of the three
   largest circuits -}
solution1 :: Boxes -> Int
solution1 boxes = product . take 3 . largest $ connectedCircuits
  where
    largest = sortOn Down . map length . toList
    connectedCircuits = fromMaybe mempty . fmap fst $ makeConnections
    makeConnections = execStateT (replicateM_ 1000 connectNearest) initState
    initState = (S.map singleton boxes, connections boxes)

{- make the shortest connection until there's only one circuit then multiply
   the x coordinates of the final connection -}
solution2 :: Boxes -> Int
solution2 boxes = fromMaybe 0 $ xProduct <$> finalConnection
  where
    xProduct ((px,_,_), (qx,_,_)) = px * qx
    finalConnection = evalStateT untilAllConnected initState
    initState = (S.map singleton boxes, connections boxes)
    untilAllConnected = do
      conn <- connectNearest
      (circuits, _) <- get
      if length circuits == 1
         then pure conn
         else untilAllConnected

-- Run both solutions
day08 :: IO ()
day08 = do
  input <- getInput "day08-input.txt"
  let solve = createSolver parser input
  solve solution1
  solve solution2
