{-# LANGUAGE OverloadedStrings #-}

module Solutions.Day05 where

import Data.Foldable
import Data.Graph (Graph, Vertex)
import Data.Graph qualified as G
import Data.List hiding ((!?))
import Data.Map (Map, (!), (!?))
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing

data PageRule n = n `Before` n
  deriving (Show, Eq, Ord)

pRule :: (Num n) => Parser (PageRule n)
pRule = Before <$> lexeme decimal <* symbol "|" <*> lexemeLn decimal

pUpdate :: (Num n) => Parser [n]
pUpdate = (lexeme decimal `sepBy` symbol ",") <* eol

pInp :: (Num n) => Parser ([PageRule n], [[n]])
pInp = do
  rules <- some pRule
  eol
  updates <- some pUpdate
  return (rules, updates)

-- | Make graph where nodes are the pages and edges signify before
mkGraph :: [PageRule Int] -> Graph
mkGraph rules = G.buildG bounds edges
  where
    nums = concat [[x, y] | x `Before` y <- rules]
    bounds = (minimum nums, maximum nums)
    edges :: [(Vertex, Vertex)]
    edges = [(x, y) | (x `Before` y) <- rules]

graphCompare :: Graph -> Int -> Int -> Ordering
graphCompare g x y
  | G.path g x y = LT
  | G.path g y x = GT
  | otherwise = EQ

middle :: [a] -> a
middle xs = xs !! mid
  where
    mid = (length xs `div` 2)

ruleSort :: [PageRule Int] -> [Int] -> [Int]
ruleSort rules updates = sortBy (graphCompare g) updates
  where
    applicableRules = [rule | rule@(x `Before` y) <- rules, x `elem` updates]
    g = mkGraph applicableRules

correctOrder :: [PageRule Int] -> [Int] -> Bool
correctOrder rules updates = updates == ruleSort rules updates

part1 :: PartSolution
part1 = Solved $ \inp -> do
  (rules, updates) <- parseIO pInp "<input>" inp
  let goodUpdates = filter (correctOrder rules) updates
  let middles = map middle goodUpdates
  return . T.show . sum $ middles

part2 :: PartSolution
part2 = Solved $ \inp -> do
  (rules, updates) <- parseIO pInp "<input>" inp
  let badUpdates = filter (not . correctOrder rules) updates
  let corrected = map (ruleSort rules) badUpdates
  let middles = map middle corrected
  return . T.show . sum $ middles

day05 :: Solution
day05 = Solution part1 part2
