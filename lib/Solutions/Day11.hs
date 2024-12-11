{-# LANGUAGE MultiWayIf #-}

module Solutions.Day11 where

import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing hiding (State)

numDigits :: Int -> Int
numDigits n = ceiling (logBase 10 (fromIntegral n + 1))

stonesAfter :: Int -> Int -> Int
stonesAfter blinks n = flip evalState M.empty (go blinks n)
  where
    memoize f blinks n = do
      res <- f blinks n
      modify $ M.insert (blinks, n) res
      return res
    go 0 _ = return 1
    go blinks n =
      gets (M.!? (blinks, n)) >>= \case
        Just alreadySeen -> return alreadySeen
        Nothing ->
          if
            | n == 0 -> memoize go (blinks - 1) 1
            | even (numDigits n) -> do
                let (n1, n2) = n `divMod` (10 ^ (numDigits n `div` 2))
                res1 <- memoize go (blinks - 1) n1
                res2 <- memoize go (blinks - 1) n2
                return $ res1 + res2
            | otherwise -> memoize go (blinks - 1) (n * 2024)

pInp :: Text -> [Int]
pInp = map readText . T.words

part1 :: PartSolution
part1 = Solved $ return . T.show . sum . map (stonesAfter 25) . pInp

part2 :: PartSolution
part2 = Solved $ return . T.show . sum . map (stonesAfter 75) . pInp

day11 :: Solution
day11 = Solution part1 part2
