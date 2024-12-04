{-# LANGUAGE OverloadedStrings #-}

module Solutions.Day04 where

import Data.List
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import SantaLib hiding (part1, part2)
import SantaLib.Parsing

key :: Text
key = "XMAS"

backwards :: Text -> Text
backwards = T.reverse

down :: Text -> Text
down = T.unlines . T.transpose . T.lines

up :: Text -> Text
up = T.reverse . down

diagonals :: [[a]] -> [[a]]
diagonals = tail . go []
  where
    go :: [[a]] -> [[a]] -> [[a]]
    go b es_ =
      map head b : case es_ of
        [] -> transpose (map (drop 1) b)
        e : es -> go (e : (map (drop 1) b)) es

countAll :: Text -> Int
countAll inp = horizontal + vertical + digs
  where
    horizontal = T.count key inp + T.count key (backwards inp)
    vertical = T.count key (down inp) + T.count key (up inp)
    strInp = lines . T.unpack $ inp
    digUpRight = diagonals strInp
    digDownLeft = map reverse digUpRight
    digUpLeft = diagonals (map reverse strInp)
    digDownRight = map reverse digUpLeft
    digs = sum [T.count key . T.pack . unlines $ dig | dig <- [digUpRight, digDownLeft, digUpLeft, digDownRight]]

findXmases :: Text -> Int
findXmases inp = length . filter (isSam matrix) $ possibleMiddles
  where
    matrix = V.fromList . map V.fromList . lines . T.unpack $ inp
    len = V.length matrix
    possibleMiddles = [(row, col) | row <- [1 .. len - 2], col <- [1 .. len - 2]]

isSam :: Vector (Vector Char) -> (Int, Int) -> Bool
isSam matrix (row, col) = (digUpRight == "SAM" || digUpRight == "MAS") && (digUpLeft == "SAM" || digUpLeft == "MAS")
  where
    middle = (matrix V.! row) V.! col
    upRight = (matrix V.! (row - 1)) V.! (col + 1)
    upLeft = (matrix V.! (row - 1)) V.! (col - 1)
    downRight = (matrix V.! (row + 1)) V.! (col + 1)
    downLeft = (matrix V.! (row + 1)) V.! (col - 1)
    digUpRight = [downLeft, middle, upRight]
    digUpLeft = [downRight, middle, upLeft]

part1 :: PartSolution
part1 = Solved $ return . T.show . countAll

part2 :: PartSolution
part2 = Solved $ return . T.show . findXmases

day04 :: Solution
day04 = Solution part1 part2
