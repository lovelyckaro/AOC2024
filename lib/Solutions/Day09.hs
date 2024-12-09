{-# LANGUAGE OverloadedRecordDot #-}

module Solutions.Day09 where

import Control.Monad
import Data.Char
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing

type FileNum = Int

type Block = Int

type Size = Int

data File = File {fileNum :: FileNum, fileSize :: Size}
  deriving (Show, Eq, Ord)

data FileSystem = FileSystem {occupied :: Map Block File, free :: Map Block Size}
  deriving (Show, Eq)

pInp :: Text -> FileSystem
pInp inp = filesystem
  where
    (filesystem, _, _) = foldl' addBlock (FileSystem M.empty M.empty, 0, 0) (zip digits (iterate not False))
    digits = map (read @Int . (: [])) $ filter isDigit $ T.unpack inp
    addBlock (fs, blockCnt, fileCnt) (size, isFree)
      | isFree = (fs {free = M.singleton blockCnt size `M.union` fs.free}, blockCnt + size, fileCnt)
      | otherwise = (fs {occupied = M.singleton blockCnt (File fileCnt size) `M.union` fs.occupied}, blockCnt + size, succ fileCnt)

splitFs :: FileSystem -> FileSystem
splitFs (FileSystem occupied free) = FileSystem splitOccupied splitFree
  where
    splitOccupied = M.fromList [(block', File file 1) | (block, File file size) <- M.assocs occupied, block' <- [block .. block + size - 1]]
    splitFree = M.fromList [(block', 1) | (block, size) <- M.assocs free, block' <- [block .. block + size - 1]]

compactStep :: FileSystem -> FileSystem
compactStep (FileSystem occupied free)
  | block > emptyBlock = FileSystem (M.delete block . M.insert emptyBlock file $ occupied) (M.delete emptyBlock . M.insert block 1 $ free)
  | otherwise = FileSystem occupied free
  where
    (block, file) = M.findMax occupied
    (emptyBlock, size) = M.findMin free

compact :: FileSystem -> FileSystem
compact fs
  | fs == fs' = fs
  | otherwise = compact fs'
  where
    fs' = compactStep fs

checksum :: FileSystem -> Int
checksum (FileSystem occupied _free) = sum [block' * file | (block, File file size) <- M.assocs occupied, block' <- [block .. block + size - 1]]

move :: FileNum -> Map FileNum Block -> FileSystem -> (Map FileNum Block, FileSystem)
move fileNum fileLocations fs@(FileSystem occupied free) = fromMaybe (fileLocations, fs) $ do
  currentBlock <- fileLocations M.!? fileNum
  f@(File _ neededSize) <- occupied M.!? currentBlock
  (newLocation, availableSize) <- M.lookupMin . M.filter (>= neededSize) $ free
  guard (newLocation < currentBlock)
  let occupied' = M.insert newLocation f . M.delete currentBlock $ occupied
  let free' = M.insert currentBlock neededSize . M.delete newLocation $ free
  let sizeLeft = availableSize - neededSize
  let free'' =
        if sizeLeft > 0
          then M.insert (newLocation + neededSize) sizeLeft free'
          else free'
  let fileLocations' = M.insert fileNum newLocation . M.delete fileNum $ fileLocations
  return (fileLocations', FileSystem occupied' free'')

mkLocations :: FileSystem -> Map FileNum Block
mkLocations (FileSystem occupied _) = M.fromList [(fileNum, block) | (block, File fileNum _) <- M.assocs occupied]

compact' :: FileSystem -> FileSystem
compact' filesystem = snd . foldl' (\(loc, fs) fileNum -> move fileNum loc fs) (fileLocations, filesystem) $ files
  where
    fileLocations = mkLocations filesystem
    files = reverse $ M.keys fileLocations

part1 :: PartSolution
part1 = Solved $ return . T.show . checksum . compact . splitFs . pInp

part2 :: PartSolution
part2 = Solved $ return . T.show . checksum . compact' . pInp

day09 :: Solution
day09 = Solution part1 part2
