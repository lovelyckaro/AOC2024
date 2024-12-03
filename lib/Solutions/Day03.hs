{-# LANGUAGE OverloadedStrings #-}

module Solutions.Day03 where

import Data.Either (rights)
import Data.Text (Text)
import Data.Text qualified as T
import Replace.Megaparsec
import SantaLib hiding (part1, part2)
import SantaLib.Parsing

data Instr n = Mul n n | Do | Dont
  deriving (Show)

pMul :: Parser (Instr Int)
pMul =
  Mul
    <$> (string "mul(" *> decimal)
    <*> (char ',' *> decimal <* char ')')

pInstr :: Parser (Instr Int)
pInstr = pMul <|> Do <$ string "do()" <|> Dont <$ string "don't()"

pMuls :: Text -> [Instr Int]
pMuls = rights . splitCap pMul

pInstrs :: Text -> [Instr Int]
pInstrs = rights . splitCap pInstr

eval :: [Instr Int] -> Int
eval = fst . foldl' handleInstr (0, True)
  where
    handleInstr (acc, False) Do = (acc, True)
    handleInstr (acc, True) Dont = (acc, False)
    handleInstr (acc, True) (Mul x y) = (acc + x * y, True)
    handleInstr (acc, enabled) instr = (acc, enabled)

part1 :: PartSolution
part1 = Solved $ return . T.show . eval . pMuls

part2 :: PartSolution
part2 = Solved $ return . T.show . eval . pInstrs

day03 :: Solution
day03 = Solution part1 part2
