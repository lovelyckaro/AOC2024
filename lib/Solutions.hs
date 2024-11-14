module Solutions
  ( module Solutions.Day01,
    module Solutions.Day02,
    module Solutions.Day03,
    module Solutions.Day04,
    module Solutions.Day05,
    module Solutions.Day06,
    module Solutions.Day07,
    module Solutions.Day08,
    module Solutions.Day09,
    module Solutions.Day10,
    module Solutions.Day11,
    module Solutions.Day12,
    module Solutions.Day13,
    module Solutions.Day14,
    module Solutions.Day15,
    module Solutions.Day16,
    module Solutions.Day17,
    module Solutions.Day18,
    module Solutions.Day19,
    module Solutions.Day20,
    module Solutions.Day21,
    module Solutions.Day22,
    module Solutions.Day23,
    module Solutions.Day24,
    solution,
  )
where

import Data.Maybe (fromJust)
import SantaLib
import Solutions.Day01
import Solutions.Day02
import Solutions.Day03
import Solutions.Day04
import Solutions.Day05
import Solutions.Day06
import Solutions.Day07
import Solutions.Day08
import Solutions.Day09
import Solutions.Day10
import Solutions.Day11
import Solutions.Day12
import Solutions.Day13
import Solutions.Day14
import Solutions.Day15
import Solutions.Day16
import Solutions.Day17
import Solutions.Day18
import Solutions.Day19
import Solutions.Day20
import Solutions.Day21
import Solutions.Day22
import Solutions.Day23
import Solutions.Day24
import Solutions.Day25

solution :: Day -> Solution
solution day =
  fromJust $
    lookup day $
      zip [mkDay_ 1 .. mkDay_ 25] [day01, day02, day03, day04, day05, day06, day07, day08, day09, day10, day11, day12, day13, day14, day15, day16, day17, day18, day19, day20, day21, day22, day23, day24, day25]
