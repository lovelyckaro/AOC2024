{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}

module Solutions.Day21 where

import Control.Monad
import Control.Monad.State
import Data.Array.IArray
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Ord
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing hiding (State)

pInp :: Text -> [Text]
pInp = T.lines

data Action = North | East | South | West | Press
  deriving (Show, Eq, Ord, Enum)

newtype DirPosition = DirPosition Action
  deriving (Show, Eq, Ord, Enum)

dirNeighbors :: DirPosition -> [(DirPosition, Action)]
dirNeighbors (DirPosition North) =
  [ (DirPosition North, Press),
    (DirPosition South, South),
    (DirPosition Press, East)
  ]
dirNeighbors (DirPosition East) =
  [ (DirPosition East, Press),
    (DirPosition South, West),
    (DirPosition Press, North)
  ]
dirNeighbors (DirPosition South) =
  [ (DirPosition South, Press),
    (DirPosition North, North),
    (DirPosition East, East),
    (DirPosition West, West)
  ]
dirNeighbors (DirPosition West) =
  [ (DirPosition West, Press),
    (DirPosition South, East)
  ]
dirNeighbors (DirPosition Press) =
  [ (DirPosition Press, Press),
    (DirPosition East, South),
    (DirPosition North, West)
  ]

type Path position = [position]

-- | Best directional keypad paths between any two positions
dirTable :: Map (DirPosition, DirPosition) [Path Action]
dirTable = M.fromListWith (<>) $ do
  from <- DirPosition <$> [North .. Press]
  (_dist, to, paths) <- pathsFrom dirNeighbors from
  return $ ((from, to), S.toList paths)

pathsFrom :: (Ord pos, Ord edge) => (pos -> [(pos, edge)]) -> pos -> ([(Int, pos, Set (Path edge))])
pathsFrom neighbors from = go S.empty (M.singleton (0, from) (S.singleton []))
  where
    go seen queue = case M.minViewWithKey queue of
      Nothing -> []
      Just (((depth, curr), paths), queue') ->
        let seen' = S.insert curr seen
            neighbs = neighbors curr
            queue'' = M.unionWith (S.union) queue' $ M.fromList [((succ depth, pos), S.map (<> [edge]) paths) | (pos, edge) <- neighbs, pos `S.notMember` seen']
         in (depth, curr, paths) : go seen' queue''

data KeypadPosition = KeypadPosition Char
  deriving (Show, Eq, Ord)

instance Enum KeypadPosition where
  fromEnum (KeypadPosition 'A') = 10
  fromEnum (KeypadPosition c)
    | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
    | otherwise = error "Bad KeypadPosition"
  toEnum n
    | n < 10 = KeypadPosition $ toEnum (n + fromEnum '0')
    | n == 10 = KeypadPosition 'A'
    | otherwise = error "Bad KeypadPosition"

instance Bounded KeypadPosition where
  minBound = KeypadPosition '0'
  maxBound = KeypadPosition 'A'

{- ORMOLU_DISABLE -}

keypadPositions :: Array (Int, Int) (Maybe KeypadPosition)
keypadPositions = listArray ((0,0), (3,2)) $ map (fmap KeypadPosition)
  [ Just '7', Just '8', Just '9',
    Just '4', Just '5', Just '6',
    Just '1', Just '2', Just '3',
    Nothing, Just '0', Just 'A'
  ]

{- ORMOLU_ENABLE -}

keypadNeighbors :: KeypadPosition -> [(KeypadPosition, Action)]
keypadNeighbors keypadPos = do
  (row, col) <- take 1 [coords | (coords, Just pos) <- assocs keypadPositions, pos == keypadPos]
  (neighbor, action) <- zip [(row - 1, col), (row, col + 1), (row + 1, col), (row, col - 1)] [North .. Press]
  keypadSlot <- maybeToList $ keypadPositions !? neighbor
  keypadNeighbor <- maybeToList keypadSlot
  return $ (keypadNeighbor, action)

-- | Best keypad paths between any two positions
keypadTable :: Map (KeypadPosition, KeypadPosition) [Path Action]
keypadTable = M.fromListWith (<>) $ do
  from <- [minBound .. maxBound]
  (_dist, to, paths) <- pathsFrom keypadNeighbors from
  return $ ((from, to), S.toList paths)

type RobotMemoM a = State (Map (Int, DirPosition, DirPosition) Int) a

runRobotMemo :: RobotMemoM a -> a
runRobotMemo = flip evalState M.empty

executePath :: Int -> Path Action -> RobotMemoM Int
executePath 0 actions = return $ length actions + 1
executePath robots actions = fst <$> foldM step (0, DirPosition Press) (actions <> [Press])
  where
    step (acc, loc) action = do
      presses <- robotPresses robots loc (DirPosition action)
      return (acc + presses, DirPosition action)

robotPresses :: Int -> DirPosition -> DirPosition -> RobotMemoM Int
robotPresses numRobots from to =
  gets (M.!? (numRobots, from, to)) >>= \case
    Just alreadyDone -> return $ alreadyDone
    Nothing -> do
      let possiblePaths = dirTable M.! (from, to)
      sequences <- mapM (executePath (numRobots - 1)) possiblePaths
      let best = minimum sequences
      modify $ M.insert (numRobots, from, to) best
      return best

keypadPresses :: Int -> KeypadPosition -> KeypadPosition -> RobotMemoM Int
keypadPresses numRobots from to = do
  let possiblePaths = keypadTable M.! (from, to)
  sequences <- mapM (executePath numRobots) possiblePaths
  return $ minimum sequences

writeCode :: Int -> Text -> RobotMemoM Int
writeCode numRobots text = fst <$> foldM step (0, KeypadPosition 'A') (T.unpack text)
  where
    step (acc, pos) next = do
      presses <- keypadPresses numRobots pos (KeypadPosition next)
      return (acc + presses, KeypadPosition next)

numericPart :: Text -> Int
numericPart = readText . fromJust . T.stripSuffix "A"

part1 :: PartSolution
part1 = Solved $ \inp -> do
  let codes = pInp inp
  let sequenceLens = runRobotMemo $ mapM (writeCode 2) codes
  let nums = map numericPart codes
  let complexity = sum $ zipWith (*) sequenceLens nums
  return . T.show $ complexity

part2 :: PartSolution
part2 = Solved $ \inp -> do
  let codes = pInp inp
  let sequenceLens = runRobotMemo $ mapM (writeCode 25) codes
  let nums = map numericPart codes
  let complexity = sum $ zipWith (*) sequenceLens nums
  return . T.show $ complexity

day21 :: Solution
day21 = Solution part1 part2
