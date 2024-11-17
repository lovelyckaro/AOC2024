{-# LANGUAGE OverloadedStrings #-}

module Solutions.Day01 where

import Control.Applicative
import Control.Monad
import Data.Char (isDigit)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing

number :: [(Text, Int)] -> Text -> Int
number digitTable line = head digs * 10 + last digs
  where
    digs = digits digitTable line

digits1 :: [(Text, Int)]
digits1 = [(T.show n, n) | n <- [1 .. 9]]

digits2 :: [(Text, Int)]
digits2 = digits1 <> zip ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] [1 .. 9]

digits :: [(Text, Int)] -> Text -> [Int]
digits digitTable nums = do
  slice <- T.tails nums
  (digitStr, digit) <- digitTable
  guard (digitStr `T.isPrefixOf` slice)
  return digit

part1 :: PartSolution
part1 = Solved $ return . T.show . sum . map (number digits1) . T.lines

part2 :: PartSolution
part2 = Solved $ return . T.show . sum . map (number digits2) . T.lines

day01 :: Solution
day01 = Solution part1 part2
