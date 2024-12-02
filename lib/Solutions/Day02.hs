module Solutions.Day02 where

import Data.List
import Data.Text (Text)
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing

pInp :: Text -> [[Int]]
pInp = map (map readText) . map T.words . T.lines

incremental :: [Int] -> Bool
incremental xs = and $ zipWith (\prev curr -> let diff = abs (curr - prev) in diff >= 1 && diff <= 3) xs (tail xs)

increases :: [Int] -> Bool
increases xs = and $ zipWith (<) xs (tail xs)

decreases :: [Int] -> Bool
decreases xs = and $ zipWith (>) xs (tail xs)

grade :: [Int] -> Bool
grade xs = incremental xs && (decreases xs || increases xs)

grade' :: [Int] -> Bool
grade' xs = go [] xs || go [] (reverse xs)
  where
    go _acc [] = True
    go _acc [x] = True
    go acc (x : y : zs)
      | x < y && y - x <= 3 && y - x >= 1 = go (x : acc) (y : zs)
      | otherwise = grade (reverse acc <> (x : zs)) || grade (reverse acc <> (y : zs))

part1 :: PartSolution
part1 = Solved $ \txt -> return . T.show . length . filter grade . pInp $ txt

part2 :: PartSolution
part2 = Solved $ \txt -> return . T.show . length . filter grade' . pInp $ txt

day02 :: Solution
day02 = Solution part1 part2
