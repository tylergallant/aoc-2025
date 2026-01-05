module Day08 (day08) where

import Control.Monad (replicateM_)
import Control.Monad.State (State, execState, get, put)
import Data.Char (isDigit)
import Data.Foldable (find)
import Data.List (sortOn, tails)
import Data.Ord (Down(..))
import Data.Set (Set, difference, fromList, insert, member, singleton, toList, union)
import qualified Data.Set as S (map)
import Text.ParserCombinators.ReadP (ReadP, char, munch1, sepBy)
import Utils (createSolver, getInput)

type Box = (Int, Int, Int)
type Connection = (Box, Box)
type Connections = [Connection]
type Circuit = Set Box
type Circuits = Set Circuit

-- Read in a set of boxes
parser :: ReadP Circuit
parser = fromList <$> sepBy box newline <* newline
  where
    box = (,,) <$> coordComma <*> coordComma <*> coord
    coordComma = coord <* char ','
    coord = read <$> munch1 isDigit
    newline = char '\n'

distance :: Connection -> Double
distance ((p1, p2, p3), (q1, q2, q3)) = sqrt $ fromIntegral diffs
  where
    diffs = sum $ square <$> zipWith (-) [p1, p2, p3] [q1, q2, q3]
    square = (^(2 :: Int))

connections :: Circuit -> [Connection]
connections = sortOn distance . pairs . toList
  where pairs items = [(x, y) | x:xs <- tails $ items, y <- xs]

connectNearest :: State (Circuits, Connections) ()
connectNearest = do
  (circuits, conns) <- get
  case conns of
    [] -> pure ()
    ((x, y):conns') -> do
      let findCircuit circuit = flip find circuits $ member circuit
      case (,) <$> findCircuit x <*> findCircuit y of
        Nothing -> put (circuits, conns')
        Just (a, b)
          | x `member` b || y `member` a -> put (circuits, conns')
          | otherwise -> do
              let without = difference circuits $ fromList [a, b]
                  with = flip insert without $ union a b
              put (with, conns')

solution1 :: Circuit -> Int
solution1 boxes = product . take 3 . largest $ connectedCircuits
  where
    conns = connections boxes
    initState = (S.map singleton boxes, conns)
    (connectedCircuits, _) = execState (replicateM_ 1000 connectNearest) initState
    largest = sortOn Down . map length . toList

day08 :: IO ()
day08 = do
  input <- getInput "day08-input.txt"
  let solve = createSolver parser input
  solve solution1
