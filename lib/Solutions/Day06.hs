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

type Graph = (Set (Int, Int), (Int, Int, Int, Int))

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
  let maxCol = S.findMax $ S.map fst obstacles
  let minCol = S.findMin $ S.map fst obstacles
  let maxRow = S.findMax $ S.map snd obstacles
  let minRow = S.findMin $ S.map snd obstacles
  return ((obstacles, (minCol, maxCol, minRow, maxRow)), (startPos, North))

step :: Position -> (Int, Int)
step ((col, row), North) = (col, row - 1)
step ((col, row), East) = (col + 1, row)
step ((col, row), South) = (col, row + 1)
step ((col, row), West) = (col - 1, row)

rotate :: Position -> Position
rotate (p, West) = (p, North)
rotate (p, dir) = (p, succ dir)

neighbors :: Graph -> Position -> Maybe Position
neighbors g@(obstacles, (minCol, maxCol, minRow, maxRow)) (p, dir) = do
  let (col', row') = step (p, dir)
  if (col', row') `S.member` obstacles
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

isLoop :: Position -> Graph -> Bool
isLoop pos g = go g S.empty pos
  where
    go g visited pos = case neighbors g pos of
      Nothing -> False
      Just n -> n `S.member` visited || go g (S.insert pos visited) n

possibleObstacleAdds :: Graph -> Position -> Set (Int, Int)
possibleObstacleAdds g pos = S.fromList $ map fst visited
  where
    visited = connected (neighbors g) pos

part2 :: PartSolution
part2 = Solved $ \inp -> do
  (graph@(obstacles, bounds), start) <- parseIO pInp "<input>" inp
  let possibleObstacles = S.delete (fst start) $ possibleObstacleAdds graph start
  let loops = foldr (\obstacle found -> if isLoop start (S.insert obstacle obstacles, bounds) then found + 1 else found) 0 possibleObstacles
  return . T.show $ loops

day06 :: Solution
day06 = Solution part1 part2
