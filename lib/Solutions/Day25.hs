{-# LANGUAGE OverloadedStrings #-}

module Solutions.Day25 where

import Control.Monad
import Data.List
import Data.Text (Text)
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing

pInp :: Text -> [String]
pInp = map (filter (/= '\n') . T.unpack) . T.splitOn "\n\n"

overlaps :: String -> String -> Bool
overlaps key lock = or $ zipWith bothBox key lock
  where
    bothBox c1 c2 = c1 == c2 && c1 == '#'

validCombinations :: [String] -> [(String, String)]
validCombinations keylocks = do
  key <- keylocks
  lock <- keylocks
  guard $ not (overlaps key lock)
  return $ (key, lock)

numCombinations :: [String] -> Int
numCombinations = (`div` 2) . length . validCombinations

part1 :: PartSolution
part1 = Solved $ return . T.show . numCombinations . pInp

part2 :: PartSolution
part2 = Unsolved

day25 :: Solution
day25 = Solution part1 part2
