module Solutions.Day12 where

import Control.Monad
import Data.Array.Unboxed
import Data.Char
import Data.Either
import Data.Function
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing

type Point = (Int, Int)

-- | Perimeter is identified by starting point and neighboring, non included point
type Perimeter = (Point, Point)

-- | Corner is identified by starting point and diaginal, non included point
type Corner = (Point, Point)

type Graph = UArray Point Char

pInp :: Text -> Graph
pInp inp = listArray ((0, 0), (rows - 1, cols - 1)) chars
  where
    chars = filter isAlpha . T.unpack $ inp
    rows = length (T.lines inp)
    cols = maximum $ map T.length (T.lines inp)

neighbPerims :: Graph -> Point -> [Either Perimeter Point]
neighbPerims graph (row, col) = do
  let wanted = graph ! (row, col)
  (row', col') <- [(row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1)]
  case graph !? (row', col') of
    Just neighbor ->
      if wanted == neighbor
        then return $ Right (row', col')
        else return $ Left ((row, col), (row', col'))
    Nothing -> return $ Left ((row, col), (row', col'))

neighbCorners :: Graph -> Point -> [Either Corner Point]
neighbCorners graph p = neighbs <> corns
  where
    neighbs = filter isRight $ neighbPerims graph p
    corns = map Left $ corners graph p

{-
.A.
D#B
.C.
-}
corners :: Graph -> Point -> [Corner]
corners graph (row, col) = do
  let wanted = graph ! (row, col)
  let outsideShape p = maybe True (/= wanted) $ graph !? p
  let innerCorner (_, outside) inside = outsideShape outside && all (not . outsideShape) inside
  let outerCorner _ outside = all outsideShape outside
  let [nw, n, ne, w, p, e, sw, s, se] = [(row + rowdiff, col + coldiff) | rowdiff <- [-1, 0, 1], coldiff <- [-1, 0, 1]]
  (corner, same) <- [((p, nw), [n, w]), ((p, ne), [n, e]), ((p, sw), [s, w]), ((p, se), [s, e])]
  guard $ innerCorner corner same || outerCorner corner same
  return corner

groupPrices :: (Point -> [Either (Point, Point) Point]) -> Graph -> Int
groupPrices neighbFun graph = go allPoints
  where
    allPoints = S.fromList $ indices graph
    go pointsLeft
      | S.null pointsLeft = 0
      | otherwise =
          let p = S.findMin pointsLeft
              (ps, pers) = group neighbFun p
           in S.size ps * length pers + go (pointsLeft S.\\ ps)

group :: (Point -> [Either (Point, Point) Point]) -> Point -> (Set Point, Set (Point, Point))
group neighbFun p = go S.empty S.empty [p]
  where
    go seen perims [] = (seen, perims)
    go seen perims (point : points) =
      let adjacent = neighbFun point
          neighbs = rights adjacent
          perims' = S.fromList $ lefts adjacent
       in go (S.insert point seen) (perims `S.union` perims') ([neighb | neighb <- neighbs, neighb `S.notMember` seen] <> points)

part1 :: PartSolution
part1 = Solved $ \inp -> do
  let graph = pInp inp
  return . T.show . groupPrices (neighbPerims graph) $ graph

part2 :: PartSolution
part2 = Solved $ \inp -> do
  let graph = pInp inp
  return . T.show . groupPrices (neighbCorners graph) $ graph

day12 :: Solution
day12 = Solution part1 part2
