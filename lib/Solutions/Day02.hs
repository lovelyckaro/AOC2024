module Solutions.Day02 where

import Data.List hiding (subsequences)
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

subsequences :: [Int] -> [[Int]]
subsequences nums = nums : zipWith (<>) (inits nums) (drop 1 $ tails nums)

grade' :: [Int] -> Bool
grade' xs = any grade $ subsequences xs

part1 :: PartSolution
part1 = Solved $ \txt -> return . T.show . length . filter grade . pInp $ txt

part2 :: PartSolution
part2 = Solved $ \txt -> return . T.show . length . filter grade' . pInp $ txt

day02 :: Solution
day02 = Solution part1 part2
