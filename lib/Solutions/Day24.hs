{-# LANGUAGE OverloadedStrings #-}

module Solutions.Day24 where

import Control.Monad.State
import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.SBV hiding (And, getAnd)
import Data.Text (Text)
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing hiding (State)

type Wire = Text

data Operator = XOr | And | Or
  deriving (Show, Eq, Ord, Enum)

data Stmt where
  Is :: Wire -> Bool -> Stmt
  Gate :: Operator -> Wire -> Wire -> Wire -> Stmt
  deriving (Show, Eq, Ord)

pWire :: Parser Wire
pWire = T.pack <$> some alphaNumChar

pIs :: Parser Stmt
pIs = do
  wire <- lexeme pWire
  symbol ":"
  set <- choice [True <$ symbol "1", False <$ symbol "0"]
  return $ wire `Is` set

pOperator :: Parser Operator
pOperator = choice [And <$ symbol "AND", Or <$ symbol "OR", XOr <$ symbol "XOR"]

pBin :: Parser Stmt
pBin = do
  w1 <- lexeme pWire
  operator <- pOperator
  w2 <- lexeme pWire
  symbol "->"
  w3 <- lexeme pWire
  return $ Gate operator w1 w2 w3

pStmt :: Parser Stmt
pStmt = choice [try pIs, pBin]

pInp :: Parser [Stmt]
pInp = some $ lexemeLn pStmt <* optional eol

getWire :: Wire -> StateT (Map Wire SBool) Symbolic SBool
getWire wire =
  gets (M.!? wire) >>= \case
    Nothing -> do
      bool <- lift $ sBool (T.unpack wire)
      modify (M.insert wire bool)
      return bool
    Just bool -> return bool

evalBin :: (SBool -> SBool -> SBool) -> Wire -> Wire -> Wire -> StateT (Map Wire SBool) Symbolic ()
evalBin operation wire1 wire2 dest = do
  b1 <- getWire wire1
  b2 <- getWire wire2
  bDest <- getWire dest
  lift $ constrain (bDest .== operation b1 b2)

evalExpr :: Stmt -> StateT (Map Wire SBool) Symbolic ()
evalExpr (wire `Is` set) = do
  bool <- getWire wire
  lift $ constrain (bool .== fromBool set)
evalExpr (Gate And wire1 wire2 dest) = evalBin (.&&) wire1 wire2 dest
evalExpr (Gate Or wire1 wire2 dest) = evalBin (.||) wire1 wire2 dest
evalExpr (Gate XOr wire1 wire2 dest) = evalBin (.<+>) wire1 wire2 dest

observeResult :: StateT (Map Wire SBool) Symbolic ()
observeResult = do
  membs <- gets M.assocs
  let zs = [val | (key, val) <- membs, "z" `T.isPrefixOf` key]
  let paddedZs = zs <> replicate (64 - length zs) sFalse
  result <- lift $ sWord64 "result"
  lift $ constrain (result .== fromBitsLE paddedZs)

makeProblem :: [Stmt] -> Symbolic ()
makeProblem stmts = evalStateT (mapM_ evalExpr stmts >> observeResult) M.empty

part1 :: PartSolution
part1 = Solved $ \inp -> do
  exprs <- parseIO pInp "<input>" inp
  res <- sat $ makeProblem exprs
  let Just result = getModelValue @_ @Word64 "result" res
  return . T.show $ result

{-
# Part 2

Circuit is supposed to be full adder.
There is no carry in.

Wanted circuit is:
x00 XOR y00 -> z00
x00 AND y00 -> carry(0)

followed by, repeated 43 times:

xN XOR yN -> aN
xN AND yN -> bN
carry(N-1) XOR aN -> zN
carry(N-1) AND aN -> cN
cN OR bN -> carry(N)

aN, bN, carryN and cN will all have random names.
Problem can be reduced to labeling the random names with their descriptive names above.
Any who cannot be labeled are wrong and can be swapped.

We can start by for example finding x00 XOR y00 -> z00
a0 = something
-}

data Adder = Adder {cout, res :: Wire}
  deriving (Show, Eq, Ord)

key (wire `Is` val) = wire
key (Gate op wire1 wire2 dest) = dest

mkTable :: [Stmt] -> Map (Operator, Wire, Wire) Wire
mkTable stmts = M.fromList [((operator, wire1, wire2), dest) | (Gate operator wire1 wire2 dest) <- stmts]

data GateError = MissingGate Operator Wire Wire
  deriving (Show)

inputNames :: Int -> (Wire, Wire)
inputNames n = ("x" <> padded, "y" <> padded)
  where
    padded = T.justifyRight 2 '0' $ T.show n

getGate :: Map (Operator, Wire, Wire) Wire -> Operator -> Wire -> Wire -> Either GateError Wire
getGate gates operator wire1 wire2 = case (gates M.!? (operator, wire1, wire2), gates M.!? (operator, wire2, wire1)) of
  (Just dest, _) -> return dest
  (_, Just dest) -> return dest
  (Nothing, Nothing) -> Left $ MissingGate operator wire1 wire2

findSwap :: Map (Operator, Wire, Wire) Wire -> Operator -> Wire -> Wire -> [(Wire, Wire)]
findSwap gates op w1 w2 = do
  ((op, w1', w2'), dest) <- M.assocs gates
  -- swap if one input and operator is correct
  case (w1', w2') of
    (w1', w2') | w1' == w1 -> return (w2', w2)
    (w1', w2') | w2' == w1 -> return (w1', w2)
    (w1', w2') | w1' == w2 -> return (w2', w1)
    (w1', w2') | w2' == w2 -> return (w1', w1)
    _ -> []

makeSwap :: (Wire, Wire) -> Map (Operator, Wire, Wire) Wire -> Map (Operator, Wire, Wire) Wire
makeSwap (w1, w2) gates = M.map swap gates
  where
    swap w = case (w == w1, w == w2) of
      (True, False) -> w2
      (False, True) -> w1
      _ -> w

getAdder :: Map (Operator, Wire, Wire) Wire -> Int -> Either GateError Adder
getAdder tables 0 = do
  let (xin, yin) = inputNames 0
  res <- getGate tables XOr xin yin
  carryOut <- getGate tables And xin yin
  return $ Adder carryOut res
getAdder tables n = do
  (Adder carryIn _) <- getAdder tables (pred n)
  let (xin, yin) = inputNames n
  an <- getGate tables XOr xin yin
  out <- getGate tables XOr carryIn an
  bn <- getGate tables And xin yin
  cn <- getGate tables And carryIn an
  carryOut <- getGate tables Or cn bn
  return $ Adder carryOut out

findSwaps :: [Stmt] -> [(Wire, Wire)]
findSwaps stmts = go [] (mkTable stmts)
  where
    go swaps gates = case getAdder gates 44 of
      Left (MissingGate operator wire1 wire2) ->
        let (swap : _otherSwaps) = findSwap gates operator wire1 wire2
            gates' = makeSwap swap gates
         in go (swap : swaps) gates'
      Right _ -> swaps

formatSwaps :: [(Wire, Wire)] -> Text
formatSwaps = T.intercalate "," . sort . concatMap (\(w1, w2) -> [w1, w2])

part2 :: PartSolution
part2 = Solved $ \inp -> do
  stmts <- parseIO pInp "<input>" inp
  let swaps = findSwaps stmts
  return . formatSwaps $ swaps

day24 :: Solution
day24 = Solution part1 part2
