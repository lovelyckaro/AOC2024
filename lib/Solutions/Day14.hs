{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Solutions.Day14 where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing
import System.Console.ANSI
import System.IO

data Robot = Robot {position, velocity :: (Int, Int)}
  deriving (Show, Eq, Ord)

pRobot :: Parser Robot
pRobot = lexemeLn $ do
  symbol "p" >> symbol "="
  x <- lexeme decimal
  symbol ","
  y <- lexeme decimal
  symbol "v" >> symbol "="
  dx <- lexeme $ signed decimal
  symbol ","
  dy <- lexeme $ signed decimal
  return $ Robot (x, y) (dx, dy)

boundX :: Int
boundX = 101

boundY :: Int
boundY = 103

posAfter :: (Int, Int) -> Int -> Robot -> (Int, Int)
posAfter (boundX, boundY) seconds (Robot (x, y) (dx, dy)) = ((x + dx * seconds) `mod` boundX, (y + dy * seconds) `mod` boundY)

frequency :: (Ord a) => [a] -> Map a Int
frequency = M.fromListWith (+) . (`zip` repeat 1)

data Quadrant = NW | NE | SE | SW
  deriving (Show, Eq, Ord)

quadrant :: (Int, Int) -> (Int, Int) -> Maybe Quadrant
quadrant (boundX, boundY) (x, y) = case (compare x (boundX `div` 2), compare y (boundY `div` 2)) of
  (EQ, _) -> Nothing
  (_, EQ) -> Nothing
  (LT, LT) -> Just NW
  (LT, GT) -> Just SW
  (GT, LT) -> Just NE
  (GT, GT) -> Just SE

pInp :: Parser [Robot]
pInp = many pRobot

part1 :: PartSolution
part1 = Solved $ \inp -> do
  robots <- parseIO pInp "<input>" inp
  return . T.show . product . frequency . mapMaybe (quadrant (boundX, boundY)) . map (posAfter (boundX, boundY) 100) $ robots

draw :: (Int, Int) -> [(Int, Int)] -> String
draw (boundX, boundY) points = unlines [[char (x, y) | x <- [0 .. boundX - 1]] | y <- [0 .. boundY - 1]]
  where
    pSet = S.fromList points
    char pos = if pos `S.member` pSet then '#' else ' '

test :: (Int, Int) -> [Robot] -> Int -> IO Int
test bounds robots second = do
  clearScreen
  let positions = map (posAfter bounds second) robots
  putStrLn $ "Second: " <> show second
  putStrLn $ draw bounds positions
  hFlush stdout
  getChar >>= \case
    'y' -> return second
    _ -> test bounds robots (second + 1)

part2 :: PartSolution
part2 = Solved $ \inp -> do
  robots <- parseIO pInp "<input>" inp
  second <- test (boundX, boundY) robots 0
  return . T.show $ second

day14 :: Solution
day14 = Solution part1 part2
