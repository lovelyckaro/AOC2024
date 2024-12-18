module Solutions.Day16 where

import Algorithm.Search
import Control.Monad
import Data.Array.IArray
import Data.Heap (MinPrioHeap)
import Data.Heap qualified as H
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing

data Square = Free | Wall | Start | End
  deriving (Show, Eq, Ord, Enum)

data Heading = North | East | South | West
  deriving (Show, Eq, Ord, Enum)

pSquare :: Parser Square
pSquare = choice $ zipWith (<$) [Free .. End] (map char ".#SE")

type Maze = Array (Int, Int) Square

pInp :: Parser Maze
pInp = do
  squares <- many (pSquare <* optional eol)
  let len = pred . floor . sqrt . fromIntegral . length $ squares
  return $ listArray ((0, 0), (len, len)) squares

type Position = ((Int, Int), Heading)

move :: Position -> Position
move ((row, col), North) = ((row - 1, col), North)
move ((row, col), East) = ((row, col + 1), East)
move ((row, col), South) = ((row + 1, col), South)
move ((row, col), West) = ((row, col - 1), West)

rotate :: Position -> Position
rotate (coords, West) = (coords, North)
rotate (coords, heading) = (coords, succ heading)

neighbors :: Maze -> Position -> [(Position, Int)]
neighbors maze pos = do
  pos'@((coords, heading), c) <- [(move pos, 1), (rotate pos, 1000), (rotate . rotate . rotate $ pos, 1000)]
  square <- maybeToList $ maze !? coords
  case square of
    Wall -> []
    _ -> return pos'

isEnd :: Maze -> Position -> Bool
isEnd maze (coords, _) =
  let [end] = [coords | (coords, End) <- assocs maze]
   in end == coords

start :: Maze -> Position
start maze =
  let [coords] = [coords | (coords, Start) <- assocs maze]
   in (coords, East)

part1 :: PartSolution
part1 = Solved $ \inp -> do
  maze <- parseIO pInp "<input>" inp
  let [end] = [coords | (coords, End) <- assocs maze]
  let isEnd = (== end) . fst
  let Just (cost, _) = dijkstraAssoc (neighbors maze) isEnd (start maze)
  return . T.show $ cost

inShortestPaths :: Maze -> (Position -> Bool) -> Int -> Map Position Int -> Set Position -> MinPrioHeap Int (Position, Set Position) -> Set Position
inShortestPaths maze isEnd knownLow seen best queue = case H.view queue of
  Nothing -> best
  Just ((cost, (position, path)), queue') ->
    let neighbs = neighbors maze position
        best' = if isEnd position then best `S.union` path else best
        seen' = M.insert position cost seen
        knownLow' = if isEnd position then cost else knownLow
        queue'' =
          foldr
            H.insert
            queue'
            [ (cost', (neighb, S.insert position path))
            | (neighb, neighbCost) <- neighbs,
              let cost' = cost + neighbCost,
              cost' < M.findWithDefault maxBound neighb seen',
              cost' <= knownLow
            ]
     in inShortestPaths maze isEnd knownLow' seen' best' queue''

part2 :: PartSolution
part2 = Solved $ \inp -> do
  maze <- parseIO pInp "<input>" inp
  let [end] = [pos | (pos, End) <- assocs maze]
  let isEnd = (== end) . fst
  let inShortest = inShortestPaths maze isEnd maxBound M.empty S.empty (H.singleton (0, (start maze, S.empty)))
  let pointsInShortest = S.map fst inShortest
  return . T.show . S.size $ pointsInShortest

day16 :: Solution
day16 = Solution part1 part2
