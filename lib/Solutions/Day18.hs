{-# LANGUAGE OverloadedStrings #-}

module Solutions.Day18 where

import Algorithm.Search
import Control.Monad
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing

pInp :: Parser [Point]
pInp = many $ lexemeLn $ (,) <$> decimal <* symbol "," <*> decimal

coordBound :: Int
coordBound = 70

coordBoundExample :: Int
coordBoundExample = 6

type Point = (Int, Int)

type Obstacles = Set Point

neighbors :: Point -> [Point]
neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

outOfBounds :: Int -> Point -> Bool
outOfBounds bound (x, y) = x < 0 || y < 0 || x > bound || y > bound

isObstacle :: Obstacles -> Point -> Bool
isObstacle obstacles p = p `S.member` obstacles

validNeighbors :: Obstacles -> Int -> Point -> [Point]
validNeighbors obstacles bound = neighbors `pruning` isObstacle obstacles `pruning` outOfBounds bound

transitionCost :: Point -> Point -> Int
transitionCost _ _ = 1

estimateCost :: Point -> Point -> Int
estimateCost (endX, endY) (x, y) = abs (endX - x) + abs (endY - y)

start :: Point
start = (0, 0)

end :: Int -> Point
end bound = (bound, bound)

part1 :: PartSolution
part1 = Solved $ \inp -> do
  points <- parseIO pInp "<input>" inp
  let obstacles = S.fromList $ take 1024 points
  let isEnd = (== end coordBound)
  let Just (cost, path) = aStar (validNeighbors obstacles coordBound) transitionCost (estimateCost (end coordBound)) isEnd start
  return . T.show $ cost

firstBlocked :: Int -> [Point] -> Int
firstBlocked bound points = binSearch 1024 (length points)
  where
    isEnd = (== end coordBound)
    isBlocked n =
      let obstacles = S.fromList $ take n points
          mbPath = bfs (validNeighbors obstacles bound) isEnd start
       in isNothing mbPath
    middle lo hi = lo + (hi - lo) `div` 2
    binSearch lower upper
      | lower >= upper = lower
      | isBlocked (middle lower upper) = binSearch lower (middle lower upper)
      | otherwise = binSearch (middle lower upper + 1) upper

part2 :: PartSolution
part2 = Solved $ \inp -> do
  points <- parseIO pInp "<input>" inp
  let blockedSecond = firstBlocked coordBound points
  let (blockerx, blockery) = last (take blockedSecond points)
  return $ T.show blockerx <> "," <> T.show blockery

day18 :: Solution
day18 = Solution part1 part2
