{-# LANGUAGE OverloadedStrings #-}

module Solutions.Day17 where

import Control.Monad
import Control.Monad.RWS
import Data.Array.IArray
import Data.Bits
import Data.Proxy
import Data.SBV hiding (listArray, succ)
import Data.Text (Text)
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing hiding (State)
import Prelude hiding (truncate)

data ComboOperand = Lit Int | RegA | RegB | RegC
  deriving (Show, Eq, Ord)

data LiteralOperand = Literal Int
  deriving (Show, Eq, Ord)

data Instr
  = Adv ComboOperand
  | Bxl LiteralOperand
  | Bst ComboOperand
  | Jnz LiteralOperand
  | Bxc
  | Out ComboOperand
  | Bdv ComboOperand
  | Cdv ComboOperand
  deriving (Show, Eq, Ord)

data ComputerState = ComputerState
  { regA, regB, regC :: Int,
    iPtr :: Int
  }
  deriving (Show, Eq, Ord)

type Program = Array Int Int

pInp :: Parser (ComputerState, Program)
pInp = do
  let iPtr = 0
  regA <- symbol "Register" >> symbol "A:" >> lexemeLn decimal
  regB <- symbol "Register" >> symbol "B:" >> lexemeLn decimal
  regC <- symbol "Register" >> symbol "C:" >> lexemeLn decimal
  eol
  symbol "Program:"
  instrs <- lexeme decimal `sepBy` symbol ","
  return (ComputerState {..}, listArray (0, length instrs - 1) instrs)

pLit :: Int -> LiteralOperand
pLit n = Literal n

pCombo :: Int -> ComboOperand
pCombo n | n <= 3 = Lit n
pCombo 4 = RegA
pCombo 5 = RegB
pCombo 6 = RegC
pCombo _ = error "Bad ComboOperand"

pInstr :: Int -> Int -> Instr
pInstr 0 operand = Adv $ pCombo operand
pInstr 1 operand = Bxl $ pLit operand
pInstr 2 operand = Bst $ pCombo operand
pInstr 3 operand = Jnz $ pLit operand
pInstr 4 _ = Bxc
pInstr 5 operand = Out $ pCombo operand
pInstr 6 operand = Bdv $ pCombo operand
pInstr 7 operand = Cdv $ pCombo operand
pInstr _ _ = error "Bad Instr"

class Operand op where
  opValue :: (MonadRWS Program [Int] ComputerState m) => op -> m Int

instance Operand ComboOperand where
  opValue (Lit n) = return n
  opValue RegA = gets regA
  opValue RegB = gets regB
  opValue RegC = gets regC

instance Operand (LiteralOperand) where
  opValue (Literal n) = return n

truncate :: Int -> Int
truncate n = n `mod` 8

nextInstr :: (MonadRWS Program [Int] ComputerState m) => m ()
nextInstr = modify $ \s -> s {iPtr = iPtr s + 2}

step :: (MonadRWS Program [Int] ComputerState m) => m ()
step = do
  i <- gets iPtr
  instr <- pInstr <$> asks (! i) <*> asks (! succ i)
  case instr of
    Adv op -> do
      num <- gets regA
      num' <- (num `shiftR`) <$> opValue op
      modify $ \s -> s {regA = num'}
      nextInstr
    Bxl op -> do
      bVal <- gets regB
      val <- opValue op
      modify $ \s -> s {regB = bVal `xor` val}
      nextInstr
    Bst op -> do
      val <- opValue op
      modify $ \s -> s {regB = truncate val}
      nextInstr
    Jnz op -> do
      aVal <- gets regA
      jumpPos <- opValue op
      if aVal == 0
        then nextInstr
        else modify $ \s -> s {iPtr = jumpPos}
    Bxc -> do
      bVal <- gets regB
      cVal <- gets regC
      modify $ \s -> s {regB = bVal `xor` cVal}
      nextInstr
    Out op -> do
      val <- opValue op
      tell [val `mod` 8]
      nextInstr
    Bdv op -> do
      num <- gets regA
      num' <- (num `shiftR`) <$> opValue op
      modify $ \s -> s {regB = num'}
      nextInstr
    Cdv op -> do
      num <- gets regA
      num' <- (num `shiftR`) <$> opValue op
      modify $ \s -> s {regC = num'}
      nextInstr

runUntilHalt :: (MonadRWS Program [Int] ComputerState m) => m ()
runUntilHalt = do
  i <- gets iPtr
  asks (!? i) >>= \case
    Nothing -> return ()
    Just _ -> step >> runUntilHalt

run :: Program -> ComputerState -> (ComputerState, [Int])
run = execRWS runUntilHalt

part1 :: PartSolution
part1 = Solved $ \inp -> do
  (initState, program) <- parseIO pInp "<input>" inp
  let (finalState, outputs) = run program initState
  let commasep = foldl' (\acc i -> acc <> "," <> T.show i) (T.show $ head outputs) (drop 1 outputs)
  return commasep

instrs :: Program -> [Instr]
instrs = go . elems
  where
    go [] = []
    go [x] = error "Odd amount of instructions"
    go (instr : op : rest) = pInstr instr op : go rest

{-
Bst RegA            # b <- a & 0b111           <--+
Bxl ( Literal 5 )   # b <- b xor 0b101            |
Cdv RegB            # c <- a / 2^b                |
Bxc                 # b <- b xor c                |
Adv ( Lit 3 )       # a <- a / 2^3                |
Bxl ( Literal 6 )   # b <- b xor 0b110            |
Out RegB            # out b                       |
Jnz ( Literal 0 )   # if a == 0 then return else -+

sbv to the rescue
-}

problem :: [SWord 3] -> Symbolic ()
problem wantedNums = do
  startA <- sWord @64 "a"
  let loopIterations = zip wantedNums (iterate (`shiftR` 3) startA)
  forM_ loopIterations $ \(wanted, a) -> do
    let b = (observe "a" a) `sMod` 8
    let b' = b `xor` 5
    let c = a `sShiftRight` b'
    let b'' = b' `xor` c
    let b''' = b'' `xor` 6
    let out = observe "out" $ bvDrop (Proxy @61) b'''
    constrain (out .== wanted)
  let (lastOut, lastA) = last loopIterations
  constrain (lastA `shiftR` 3 .== 0)
  minimize "input" startA

part2 :: PartSolution
part2 = Solved $ \inp -> do
  (_, program) <- parseIO pInp "<input>" inp
  let words = map fromIntegral $ elems program
  solution <- optLexicographic (problem words)
  print $ LexicographicResult solution
  let Just a = getModelValue @_ @Word64 "input" solution
  return . T.show $ a

day17 :: Solution
day17 = Solution part1 part2
