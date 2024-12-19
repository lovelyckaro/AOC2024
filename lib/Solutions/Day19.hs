{-# LANGUAGE OverloadedStrings #-}

module Solutions.Day19 where

import Control.Monad
import Control.Monad.State
import Data.Char
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as M
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing hiding (State)

pInp :: Parser ([Text], [Text])
pInp = do
  let pTowel = (takeWhile1P (Just "towel color") (`T.elem` "wubrg"))
  baseTowels <- lexeme pTowel `sepBy` symbol ","
  eol >> eol
  wantedTowels <- some (lexemeLn pTowel)
  eof
  return $ (baseTowels, wantedTowels)

canMake :: [Text] -> Text -> Bool
canMake baseTowels T.Empty = True
canMake baseTowels towel = or $ do
  nextTowel <- baseTowels
  case T.stripPrefix nextTowel towel of
    Nothing -> []
    Just towel' -> return $ canMake baseTowels towel'

waysToMake :: (MonadState (HashMap Text Int) m) => [Text] -> Text -> m Int
waysToMake baseTowels T.Empty = return 1
waysToMake baseTowels towel =
  gets (M.lookup towel) >>= \case
    Just seen -> return seen
    Nothing ->
      sum <$> do
        let nextSteps = mapMaybe (`T.stripPrefix` towel) baseTowels
        forM nextSteps $ \subTowel -> do
          wtm <- waysToMake baseTowels subTowel
          modify $ M.insert subTowel wtm
          return wtm

part1 :: PartSolution
part1 = Solved $ \inp -> do
  (baseTowels, wantedTowels) <- parseIO pInp "<input>" inp
  let makeable = filter (canMake baseTowels) wantedTowels
  return . T.show . length $ makeable

part2 :: PartSolution
part2 = Solved $ \inp -> do
  (baseTowels, wantedTowels) <- parseIO pInp "<input>" inp
  let makeableWays = evalState (mapM (waysToMake baseTowels) wantedTowels) M.empty
  return . T.show . sum $ makeableWays

day19 :: Solution
day19 = Solution part1 part2
