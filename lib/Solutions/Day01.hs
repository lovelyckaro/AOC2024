module Solutions.Day01 where

import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing

pInp :: Text -> ([Int], [Int])
pInp txt = (ids1, ids2)
  where
    ls = T.lines txt
    words = T.words <$> ls
    ids1 = readText . (!! 0) <$> words
    ids2 = readText . (!! 1) <$> words

part1 :: PartSolution
part1 = Solved $ \txt -> do
  let (ids1, ids2) = pInp txt
  return . T.show . sum $ map abs $ zipWith (-) (sort ids1) (sort ids2)

frequency :: [Int] -> Map Int Int
frequency nums = M.fromListWith (+) (zip nums (repeat 1))

part2 :: PartSolution
part2 = Solved $ \txt -> do
  let (ids1, ids2) = pInp txt
  let freq = frequency ids2
  return . T.show . sum . map (\n -> n * M.findWithDefault 0 n freq) $ ids1

day01 :: Solution
day01 = Solution part1 part2
