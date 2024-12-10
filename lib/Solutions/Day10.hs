module Solutions.Day10 where

import Control.Monad
import Data.Array.Unboxed
import Data.Char
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing

pInp :: Text -> UArray (Int, Int) Int
pInp inp = listArray ((0, 0), (rows - 1, cols - 1)) digits
  where
    digits = map (\d -> read [d]) . filter isDigit . T.unpack $ inp
    rows = length (T.lines inp)
    cols = maximum $ map T.length (T.lines inp)

reachableNines :: UArray (Int, Int) Int -> (Int, Int) -> Set (Int, Int)
reachableNines graph (row, col) = case graph !? (row, col) of
  Nothing -> S.empty
  Just 9 -> S.singleton (row, col)
  Just currentHeight -> S.unions $ do
    neighbor <- [(row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1)]
    neighborHeight <- maybeToList $ graph !? neighbor
    guard (neighborHeight - currentHeight == 1)
    return $ reachableNines graph neighbor

ninePaths :: UArray (Int, Int) Int -> (Int, Int) -> [[(Int, Int)]]
ninePaths graph (row, col) = case graph !? (row, col) of
  Nothing -> []
  Just 9 -> return [(row, col)]
  Just currentHeight ->
    ((row, col) :) <$> do
      neighbor <- [(row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1)]
      neighborHeight <- maybeToList $ graph !? neighbor
      guard (neighborHeight - currentHeight == 1)
      ninePaths graph neighbor

trailheads :: UArray (Int, Int) Int -> [(Int, Int)]
trailheads graph = [p | (p, 0) <- assocs graph]

part1 :: PartSolution
part1 = Solved $ \inp -> do
  let graph = pInp inp
  let starts = trailheads graph
  return . T.show . sum . map (S.size . reachableNines graph) $ starts

part2 :: PartSolution
part2 = Solved $ \inp -> do
  let graph = pInp inp
  let starts = trailheads graph
  return . T.show . sum . map (length . ninePaths graph) $ starts

day10 :: Solution
day10 = Solution part1 part2
