module Solutions.Day20 where

import Control.Monad
import Data.Array.IArray
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing

data Square = Free | Wall | Start | End
  deriving (Show, Eq, Ord, Enum)

pSquare :: Parser Square
pSquare = choice $ zipWith (<$) [Free .. End] (map char ".#SE")

type Maze = Array (Int, Int) Square

pInp :: Parser Maze
pInp = do
  squares <- many (pSquare <* optional eol)
  let len = pred . floor . sqrt . fromIntegral . length $ squares
  return $ listArray ((0, 0), (len, len)) squares

neighbors :: Maze -> (Int, Int) -> [(Int, Int)]
neighbors maze (row, col) = do
  neighb <- [(row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1)]
  square <- maybeToList $ maze !? neighb
  guard (square /= Wall)
  return neighb

start :: Maze -> (Int, Int)
start maze = startPos
  where
    [startPos] = [pos | (pos, Start) <- assocs maze]

end :: Maze -> (Int, Int)
end maze = endPos
  where
    [endPos] = [pos | (pos, End) <- assocs maze]

distances :: Maze -> (Int, Int) -> Map (Int, Int) Int
distances maze point = go M.empty [(point, 0)]
  where
    go seen [] = seen
    go seen ((point, dist) : queue) =
      let neighbs = [(neighb, dist + 1) | neighb <- neighbors maze point, neighb `M.notMember` seen]
          seen' = M.insert point dist seen
          queue' = queue <> neighbs
       in go seen' queue'

manDist :: (Int, Int) -> (Int, Int) -> Int
manDist (row, col) (row', col') = abs (row - row') + abs (col - col')

cheatNeighbors :: Maze -> Int -> (Int, Int) -> [(Int, Int)]
cheatNeighbors maze allowedDist (row, col) = do
  neighb <- [(row + drow, col + dcol) | drow <- [-allowedDist .. allowedDist], dcol <- [-allowedDist .. allowedDist], abs drow + abs dcol <= allowedDist]
  square <- maybeToList $ maze !? neighb
  guard (square /= Wall)
  return neighb

possibleCheats :: Maze -> Int -> Map (Int, Int) Int -> Map (Int, Int) Int -> (Int, Int) -> [Int]
possibleCheats maze allowedDist startDists endDists point = do
  neighb <- cheatNeighbors maze allowedDist point
  let distHere = startDists M.! point
  let distHereToThere = manDist point neighb
  let distThere = endDists M.! neighb
  let cheatDist = distHere + distHereToThere + distThere
  let totalDist = startDists M.! point + endDists M.! point
  guard (cheatDist < totalDist)
  return $ totalDist - cheatDist

cheatTimes :: Maze -> Int -> Map Int Int
cheatTimes maze allowedDist = M.fromListWith (+) $ zip cheats (repeat 1)
  where
    cheats = M.keys startDists >>= possibleCheats maze allowedDist startDists endDists
    startDists = distances maze (start maze)
    endDists = distances maze (end maze)

part1 :: PartSolution
part1 = Solved $ \inp -> do
  maze <- parseIO pInp "<input>" inp
  let times = cheatTimes maze 2
  return . T.show . sum . M.filterWithKey (\k _ -> k >= 100) $ times

part2 :: PartSolution
part2 = Solved $ \inp -> do
  maze <- parseIO pInp "<input>" inp
  let times = cheatTimes maze 20
  return . T.show . sum . M.filterWithKey (\k _ -> k >= 100) $ times

day20 :: Solution
day20 = Solution part1 part2
