module Solutions.Day15 where

import Control.Monad
import Control.Monad.ST
import Data.Array.IArray
import Data.Array.ST
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing

data Move = North | East | South | West
  deriving (Show, Eq, Ord, Enum)

pMove :: Parser Move
pMove = choice $ zipWith (<$) [North .. West] [char c | c <- "^>v<"]

data Square = Free | Box | Wall | Robot
  deriving (Show, Eq, Ord, Enum)

pSquare :: Parser Square
pSquare = choice $ zipWith (<$) [Free .. Robot] [char c | c <- ".O#@"]

pGraph :: Parser (Array (Int, Int) Square, (Int, Int))
pGraph = do
  squares <- many (pSquare <* void (optional eol))
  let dim = pred . floor . sqrt . fromIntegral . length $ squares
  let arr = listArray ((0, 0), (dim, dim)) squares
  let [startPosition] = [pos | (pos, Robot) <- assocs arr]
  return (arr, startPosition)

pMoves :: Parser [Move]
pMoves = many (pMove <* void (optional eol))

pInp :: Parser (Array (Int, Int) Square, (Int, Int), [Move])
pInp = do
  (graph, start) <- pGraph
  eol
  moves <- pMoves
  return $ (graph, start, moves)

nextPos :: (Int, Int) -> Move -> (Int, Int)
nextPos (row, col) North = (row - 1, col)
nextPos (row, col) East = (row, col + 1)
nextPos (row, col) South = (row + 1, col)
nextPos (row, col) West = (row, col - 1)

attemptMove :: STArray s (Int, Int) Square -> (Int, Int) -> Move -> ST s (Maybe (Int, Int))
attemptMove arr pos move = do
  curr <- readArray arr pos
  let pos' = nextPos pos move
  readArray arr pos' >>= \case
    Wall -> return Nothing
    Free -> do
      writeArray arr pos' curr
      writeArray arr pos Free
      return $ Just pos'
    Box ->
      attemptMove arr pos' move >>= \case
        Nothing -> return Nothing
        Just _ -> do
          writeArray arr pos' curr
          writeArray arr pos Free
          return $ Just pos'
    Robot -> error "the impossible happened"

performMoves :: Array (Int, Int) Square -> (Int, Int) -> [Move] -> Array (Int, Int) Square
performMoves startingArr startingPos moves = runSTArray $ do
  arr <- thaw startingArr
  let step pos move = maybe pos id <$> attemptMove arr pos move
  foldM step startingPos moves
  return arr

score :: Array (Int, Int) Square -> Int
score arr = sum [row * 100 + col | ((row, col), Box) <- assocs arr]

part1 :: PartSolution
part1 = Solved $ \inp -> do
  (arr, startingPos, moves) <- parseIO pInp "<input>" inp
  let arr' = performMoves arr startingPos moves
  return . T.show . score $ arr'

data Square2 = Free2 | LeftBox | RightBox | Wall2 | Robot2
  deriving (Show, Eq, Ord, Enum)

expand :: Array (Int, Int) Square -> Array (Int, Int) Square2
expand arr = array (lower, (rows, cols * 2 + 1)) (assocs arr >>= expandSquare)
  where
    (lower, (rows, cols)) = bounds arr
    expandSquare ((row, col), Free) = [((row, 2 * col), Free2), ((row, 2 * col + 1), Free2)]
    expandSquare ((row, col), Box) = [((row, 2 * col), LeftBox), ((row, 2 * col + 1), RightBox)]
    expandSquare ((row, col), Wall) = [((row, 2 * col), Wall2), ((row, 2 * col + 1), Wall2)]
    expandSquare ((row, col), Robot) = [((row, 2 * col), Robot2), ((row, 2 * col + 1), Free2)]

canMove :: STArray s (Int, Int) Square2 -> (Int, Int) -> Move -> ST s Bool
canMove arr pos move = do
  let pos' = nextPos pos move
  readArray arr pos' >>= \case
    Free2 -> return True
    Wall2 -> return False
    LeftBox -> do
      let rightPos = (fst pos', snd pos' + 1)
      leftCanMove <- canMove arr pos' move
      rightCanMove <-
        if rightPos == pos
          then return True
          else canMove arr (fst pos', snd pos' + 1) move
      return $ leftCanMove && rightCanMove
    RightBox -> do
      let leftPos = (fst pos', snd pos' - 1)
      leftCanMove <-
        if leftPos == pos
          then return True
          else canMove arr leftPos move
      rightCanMove <- canMove arr pos' move
      return $ leftCanMove && rightCanMove
    Robot2 -> canMove arr pos' move

performMove :: STArray s (Int, Int) Square2 -> (Int, Int) -> Move -> ST s ()
performMove arr pos move = do
  let pos' = nextPos pos move
  curr <- readArray arr pos
  readArray arr pos' >>= \case
    Free2 -> do
      writeArray arr pos' curr
    Wall2 -> error "impossible"
    LeftBox -> do
      performMove arr (fst pos', snd pos' + 1) move
      performMove arr pos' move
      writeArray arr pos' curr
    RightBox -> do
      performMove arr (fst pos', snd pos' - 1) move
      performMove arr pos' move
      writeArray arr pos' curr
    Robot2 -> error "impossible"
  writeArray arr pos Free2

attemptMove2 :: STArray s (Int, Int) Square2 -> (Int, Int) -> Move -> ST s (Int, Int)
attemptMove2 arr pos move = do
  moveable <- canMove arr pos move
  when moveable $ performMove arr pos move
  if moveable
    then return $ nextPos pos move
    else return $ pos

performMoves2 :: Array (Int, Int) Square2 -> (Int, Int) -> [Move] -> Array (Int, Int) Square2
performMoves2 startingArr startingPos moves = runSTArray $ do
  arr <- thaw startingArr
  let step pos move = attemptMove2 arr pos move
  foldM step startingPos moves
  return arr

score2 :: Array (Int, Int) Square2 -> Int
score2 arr = sum [row * 100 + col | ((row, col), LeftBox) <- assocs arr]

part2 :: PartSolution
part2 = Solved $ \inp -> do
  (arr, (startRow, startCol), moves) <- parseIO pInp "<input>" inp
  let arr' = expand arr
  let start = (startRow, startCol * 2)
  let arr'' = performMoves2 arr' start moves
  return . T.show . score2 $ arr''

day15 :: Solution
day15 = Solution part1 part2
