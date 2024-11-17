module Solutions
  ( solution,
  )
where

import Data.Maybe (fromJust)
import SantaLib
import Solutions.Day01 (day01)
import Solutions.Day02 (day02)
import Solutions.Day03 (day03)
import Solutions.Day04 (day04)
import Solutions.Day05 (day05)
import Solutions.Day06 (day06)
import Solutions.Day07 (day07)
import Solutions.Day08 (day08)
import Solutions.Day09 (day09)
import Solutions.Day10 (day10)
import Solutions.Day11 (day11)
import Solutions.Day12 (day12)
import Solutions.Day13 (day13)
import Solutions.Day14 (day14)
import Solutions.Day15 (day15)
import Solutions.Day16 (day16)
import Solutions.Day17 (day17)
import Solutions.Day18 (day18)
import Solutions.Day19 (day19)
import Solutions.Day20 (day20)
import Solutions.Day21 (day21)
import Solutions.Day22 (day22)
import Solutions.Day23 (day23)
import Solutions.Day24 (day24)
import Solutions.Day25 (day25)

solution :: Day -> Solution
solution day =
  fromJust $
    lookup day $
      zip [mkDay_ 1 .. mkDay_ 25] [day01, day02, day03, day04, day05, day06, day07, day08, day09, day10, day11, day12, day13, day14, day15, day16, day17, day18, day19, day20, day21, day22, day23, day24, day25]
