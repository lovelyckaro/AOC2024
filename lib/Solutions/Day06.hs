{-# LANGUAGE OverloadedRecordDot #-}

module Solutions.Day06 where

import Control.Monad
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing
import Text.Megaparsec.Char.Lexer qualified as Lex

type Graph = Set (Int, Int)

pos :: Parser (Int, Int)
pos = do
  p <- getSourcePos
  return (unPos p.sourceColumn, unPos p.sourceLine)

graphSpace :: Parser ()
graphSpace = do
  many $ void (char '.') <|> void eol
  return ()

data Direction = North | East | South | West
  deriving (Show, Eq, Ord, Enum)

type Position = ((Int, Int), Direction)

pInp :: Parser (Graph, Position)
pInp = do
  graphSpace
  nodes <- some $ Lex.lexeme graphSpace $ do
    p <- pos
    c <- choice [char '#', char '^']
    return (p, c)
  let obstacles = S.fromList . map fst . filter ((== '#') . snd) $ nodes
  let [(startPos, '^')] = filter ((== '^') . snd) nodes
  return (obstacles, (startPos, North))

step :: Position -> (Int, Int)
step ((col, row), North) = (col, row - 1)
step ((col, row), East) = (col + 1, row)
step ((col, row), South) = (col, row + 1)
step ((col, row), West) = (col - 1, row)

rotate :: Position -> Position
rotate (p, West) = (p, North)
rotate (p, dir) = (p, succ dir)

neighbors :: Graph -> Position -> Maybe Position
neighbors g (p, dir) = do
  let (col', row') = step (p, dir)
  let maxCol = S.findMax $ S.map fst g
  let minCol = S.findMin $ S.map fst g
  let maxRow = S.findMax $ S.map snd g
  let minRow = S.findMin $ S.map snd g
  if (col', row') `S.member` g
    then neighbors g (rotate (p, dir))
    else do
      guard (col' <= maxCol && col' >= minCol)
      guard (row' <= maxRow && row' >= minRow)
      return ((col', row'), dir)

part1 :: PartSolution
part1 = Solved $ \inp -> do
  (graph, start) <- parseIO pInp "<input>" inp
  let visited = connected (neighbors graph) start
  let visitedPoints = S.fromList $ map fst visited
  return . T.show . S.size $ visitedPoints

{-
Create loops:
brute force that shit, we have all the points the guard visits
from the first puzzle.
Replace them one by one with obstacles and detect if they loop
-}

isLoop :: Position -> Graph -> Bool
isLoop pos g = go g S.empty pos
  where
    go g visited pos
      | next == Nothing = False
      | fromJust next `S.member` visited = True
      | otherwise = go g (S.insert pos visited) (fromJust next)
      where
        next = neighbors g pos

possibleObstacleAdds :: Graph -> Position -> [(Int, Int)]
possibleObstacleAdds g pos = S.toList $ S.fromList $ map fst visited
  where
    visited = connected (neighbors g) pos

alternateGraphs :: Graph -> [(Int, Int)] -> [Graph]
alternateGraphs g points = map (`S.insert` g) points

part2 :: PartSolution
part2 = Solved $ \inp -> do
  (graph, start) <- parseIO pInp "<input>" inp
  let possibleObstacles = possibleObstacleAdds graph start
  let loops = foldr (\obstacle found -> if isLoop start (S.insert obstacle graph) then found + 1 else found) 0 possibleObstacles
  return . T.show $ loops

day06 :: Solution
day06 = Solution part1 part2
