{-# LANGUAGE OverloadedStrings #-}

module Solutions.Day13 where

import Data.Maybe
import Data.SBV
import Data.SBV.Tools.Overflow
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing

data ClawMachine i = ClawMachine {buttonA :: (i, i), buttonB :: (i, i), prizePosition :: (i, i)}
  deriving (Show, Eq, Ord)

pClawMachine :: (Num i) => Parser (ClawMachine i)
pClawMachine = ClawMachine <$> pButton "A" <*> pButton "B" <*> pPrize
  where
    pButton buttonName = lexemeLn $ do
      symbol "Button"
      symbol buttonName
      symbol ":"
      symbol "X" >> symbol "+"
      x <- decimal
      symbol ","
      symbol "Y" >> symbol "+"
      y <- decimal
      return (x, y)
    pPrize = lexemeLn $ do
      symbol "Prize" >> symbol ":"
      symbol "X" >> symbol "="
      x <- decimal
      symbol ","
      symbol "Y" >> symbol "="
      y <- decimal
      return (x, y)

pInp :: (Num n) => Parser [ClawMachine n]
pInp = pClawMachine `sepBy` eol

minCostProblem :: Maybe SInteger -> ClawMachine SInteger -> Symbolic ()
minCostProblem maxPresses (ClawMachine (adx, ady) (bdx, bdy) (px, py)) = do
  [aPresses, bPresses] <- sIntegers ["aPresses", "bPresses"]
  let x = adx * aPresses + bdx * bPresses
  let y = ady * aPresses + bdy * bPresses
  case maxPresses of
    Nothing -> return ()
    Just max -> do
      constrain (aPresses .<= max)
      constrain (bPresses .<= max)
  constrain (x .== px)
  constrain (y .== py)
  minimize "cost" (aPresses * 3 + bPresses)

minCost :: Maybe SInteger -> ClawMachine SInteger -> IO (Maybe Integer)
minCost maxPresses clawmachine = getModelValue "cost" <$> optLexicographic (minCostProblem maxPresses clawmachine)

correctDim :: (Num n) => ClawMachine n -> ClawMachine n
correctDim (ClawMachine ba bb (x, y)) = (ClawMachine ba bb (dim + x, dim + y))
  where
    dim = 10000000000000

part1 :: PartSolution
part1 = Solved $ \inp -> do
  machines <- parseIO pInp "<input>" inp
  costs <- catMaybes <$> mapM (minCost (Just 100)) machines
  return . T.show . sum $ costs

part2 :: PartSolution
part2 = Solved $ \inp -> do
  machines <- map correctDim <$> parseIO pInp "<input>" inp
  costs <- catMaybes <$> mapM (minCost Nothing) machines
  return . T.show . sum $ costs

day13 :: Solution
day13 = Solution part1 part2
