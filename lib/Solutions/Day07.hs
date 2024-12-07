{-# LANGUAGE OverloadedStrings #-}

module Solutions.Day07 where

import Control.Monad
import Data.List
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing
import Prelude hiding ((||))

pInp :: (Num n) => Parser [(n, [n])]
pInp = many $ lexemeLn $ do
  wanted <- lexeme decimal
  symbol ":"
  nums <- some $ lexeme decimal
  return (wanted, nums)

eval :: (Num n, Eq n, Ord n) => [(n -> n -> n)] -> (n, [n]) -> Bool
eval operators (wanted, nums) =
  any (== wanted) $
    foldl' evalNum (take 1 nums) (drop 1 nums)
  where
    evalNum accs num = do
      operator <- operators
      acc <- accs
      let next = acc `operator` num
      -- acc is only increasing, no use to keep going if larger than wanted
      guard (next <= wanted)
      return $ next

(||) :: (Num n, Integral n) => n -> n -> n
x || y = x * (10 ^ (succ . floor . logBase 10 . fromIntegral $ y)) + y

part1 :: PartSolution
part1 = Solved $ \inp -> do
  rules <- parseIO (pInp @Int) "<input>" inp
  let goodRules = filter (eval [(+), (*)]) rules
  return . T.show . sum . map fst $ goodRules

part2 :: PartSolution
part2 = Solved $ \inp -> do
  rules <- parseIO (pInp @Int) "<input>" inp
  let goodRules = filter (eval [(+), (*), (||)]) rules
  return . T.show . sum . map fst $ goodRules

day07 :: Solution
day07 = Solution part1 part2
