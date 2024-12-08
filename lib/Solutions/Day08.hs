{-# LANGUAGE OverloadedRecordDot #-}

module Solutions.Day08 where

import Control.Monad
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing
import Text.Megaparsec.Char.Lexer qualified as Lex

pos :: Parser (Int, Int)
pos = do
  p <- getSourcePos
  return (unPos p.sourceColumn, unPos p.sourceLine)

graphSpace :: Parser ()
graphSpace = do
  many $ void (char '.') <|> void eol
  return ()

pInp :: Parser (Map Char [(Int, Int)], (Int, Int))
pInp = do
  graphSpace
  points <- some $ Lex.lexeme graphSpace $ do
    p <- pos
    c <- alphaNumChar
    return (c, [p])
  eof
  (1, n) <- pos
  return $ (M.fromListWith (<>) points, (n - 1, n - 1))

inBounds (maxX, maxY) (x, y) = x <= maxX && y <= maxY && x > 0 && y > 0

antinodes :: (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
antinodes bounds (x, y) (x', y') = [p' | p' <- [(x + xdiff, y + ydiff), (x' - xdiff, y' - ydiff)], inBounds bounds p']
  where
    xdiff = x - x'
    ydiff = y - y'

antinodes' :: (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
antinodes' bounds (x, y) (x', y') = before <> after
  where
    xdiff = x - x'
    ydiff = y - y'
    before = takeWhile (inBounds bounds) [(x + step * xdiff, y + step * ydiff) | step <- [0, -1 ..]]
    after = takeWhile (inBounds bounds) [(x + step * xdiff, y + step * ydiff) | step <- [1 ..]]

allAntinodes :: ((Int, Int) -> (Int, Int) -> [(Int, Int)]) -> Map Char [(Int, Int)] -> Set (Int, Int)
allAntinodes anodes antennas = S.unions (M.map nodes antennas)
  where
    nodes as = S.fromList $ do
      a <- as
      b <- as
      guard (a /= b)
      anodes a b

part1 :: PartSolution
part1 = Solved $ \inp -> do
  (antennas, bounds) <- parseIO pInp "<input>" inp
  return . T.show . S.size $ allAntinodes (antinodes bounds) antennas

part2 :: PartSolution
part2 = Solved $ \inp -> do
  (antennas, bounds) <- parseIO pInp "<input>" inp
  return . T.show . S.size $ allAntinodes (antinodes' bounds) antennas

day08 :: Solution
day08 = Solution part1 part2
