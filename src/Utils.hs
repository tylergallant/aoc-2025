module Utils (createSolver, getInput) where

import Data.Maybe (listToMaybe)
import Paths_aoc2025 (getDataFileName)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)

runSolution :: ReadP input -> String -> (input -> output) -> Maybe output
runSolution parser input solution = processResult $ parseAndSolve input
  where
    parseAndSolve = readP_to_S $ solution <$> parser
    processResult = listToMaybe . fmap fst . filter fullParses
    fullParses = null . snd

getInput :: FilePath -> IO String
getInput filename = do
  inputFileName <- getDataFileName filename
  readFile inputFileName

createSolver
  :: Show output
  => ReadP input
  -> String
  -> (input -> output)
  -> IO ()
createSolver parser input = foldMap print . runSolution parser input
