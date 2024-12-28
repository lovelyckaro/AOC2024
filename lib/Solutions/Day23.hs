{-# LANGUAGE OverloadedStrings #-}

module Solutions.Day23 where

import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.Containers.ListUtils
import Data.Graph.Inductive
import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Ord
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing hiding (State, empty)

type Computer = Text

pInp :: Parser [(Computer, Computer)]
pInp = some $ lexemeLn $ (,) <$> pComp <* char '-' <*> pComp
  where
    pComp = takeWhile1P (Just "computer name") isLower

buildGraph :: [(Computer, Computer)] -> (NodeMap Computer, Gr Computer Int)
buildGraph connections = snd $ second (second undir) $ run empty $ do
  let computers = nubOrd [computer | (c1, c2) <- connections, computer <- [c1, c2]]
  mapM_ insMapNodeM computers
  mapM_ insMapEdgeM [(c1, c2, 1) | (c1, c2) <- connections]

cliques :: Gr Computer Int -> Set (Set Node)
cliques graph = bronKerbosch initR initP initX
  where
    initP = (S.fromList $ nodes graph)
    initX = S.empty
    initR = S.empty
    -- \| solves max cliques problem, expensive.
    -- https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm
    bronKerbosch :: Set Node -> Set Node -> Set Node -> Set (Set Node)
    bronKerbosch r p x = case S.minView p of
      Nothing -> if S.null x then S.singleton r else S.empty
      Just (v, p') ->
        let neighbs = S.fromList $ neighbors graph v
            found = bronKerbosch (S.insert v r) (p `S.intersection` neighbs) (x `S.intersection` neighbs)
         in found <> bronKerbosch r p' (S.insert v x)

tNodes :: Gr Computer Int -> Set Node
tNodes graph = S.fromList [node | node <- nodes graph, let Just name = lab graph node, "t" `T.isPrefixOf` name]

ttriangles :: Gr Computer Int -> Set (Set Node)
ttriangles graph = S.unions $ S.map nodeTriangles ts
  where
    ts = tNodes graph
    nodeTriangles node = S.fromList $ do
      neighb <- neighbors graph node
      neighb' <- neighbors graph neighb
      guard (neighb /= neighb')
      guard (neighb' /= node)
      guard (graph `hasEdge` (node, neighb'))
      return $ S.fromList [node, neighb, neighb']

part1 :: PartSolution
part1 = Solved $ \inp -> do
  connections <- parseIO pInp "<input>" inp
  let (nodes, graph) = buildGraph connections
  return . T.show . S.size . ttriangles $ graph

formatStr :: Gr Computer Int -> Set Node -> Text
formatStr graph clique = T.intercalate "," $ sort $ catMaybes [lab graph node | node <- S.toList clique]

part2 :: PartSolution
part2 = Solved $ \inp -> do
  connections <- parseIO pInp "<input>" inp
  let (nodes, graph) = buildGraph connections
  let largestClique = maximumBy (comparing S.size) $ cliques graph
  return . formatStr graph $ largestClique

day23 :: Solution
day23 = Solution part1 part2
