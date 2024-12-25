module Solutions.Day22 where

import Data.Bits
import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing

pInp :: (Read n) => Text -> [n]
pInp = map readText . T.lines

next :: (Integral n, Num n, Bits n) => n -> n
next secret =
  let secret' = secret * 64 |> mix secret |> prune
      secret'' = (secret' `div` 32) |> mix secret' |> prune
      secret''' = secret'' * 2048 |> mix secret'' |> prune
   in secret'''

prune :: (Integral n) => n -> n
prune = (`mod` 16777216)

mix :: (Bits n) => n -> n -> n
mix x1 x2 = x1 `xor` x2

secrets :: (Integral n, Num n, Bits n) => n -> [n]
secrets = iterate next

prices :: (Integral n, Num n, Bits n) => n -> [n]
prices = map (`mod` 10) . secrets

changes :: (Num n) => [n] -> [n]
changes ns = zipWith (-) (tail ns) ns

priceSeqs :: (Integral n, Num n, Bits n) => n -> [(n, (n, n, n, n))]
priceSeqs n = zip (drop 4 ps) (zip4 cs (drop 1 cs) (drop 2 cs) (drop 3 cs))
  where
    ps = prices n
    cs = take 2000 $ changes ps

findSeq :: (Eq n) => [(n, (n, n, n, n))] -> (n, n, n, n) -> Maybe (n, (n, n, n, n))
findSeq pseqs wanted = find ((== wanted) . snd) pseqs

firstNums :: (Ord n) => [(n, (n, n, n, n))] -> Map (n, n, n, n) n
firstNums ps = M.fromListWith (\new old -> old) [(seq, num) | (num, seq) <- ps]

bananasByRule :: (Ord n, Bits n, Integral n, Num n) => [n] -> Map (n, n, n, n) n
bananasByRule = M.unionsWith (+) . map (firstNums . priceSeqs)

part1 :: PartSolution
part1 = Solved $ return . T.show . sum . map (\n -> secrets n !! 2000) . pInp @Int

part2 :: PartSolution
part2 = Solved $ return . T.show . maximum . bananasByRule . pInp @Int

day22 :: Solution
day22 = Solution part1 part2
