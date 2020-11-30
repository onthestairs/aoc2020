{-# LANGUAGE NoImplicitPrelude #-}

module Run where

import AOC
import Control.Lens
import qualified Data.Map.Strict as Map
import Day1
import Relude

solves =
  Map.fromList
    [ (1, SimpleSolution Day1.solution)
    ]

data Part = Part1 | Part2 | Both

peekInput :: Int -> IO ()
peekInput day = do
  let maybeSolution = Map.lookup day solves
  case maybeSolution of
    Nothing -> putStrLn "No solution"
    Just (SimpleSolution solution) -> do
      parsed <- view parse solution
      print parsed

solve :: Int -> Part -> IO ()
solve day part = do
  let maybeSolution = Map.lookup day solves
  case maybeSolution of
    Nothing -> putStrLn "No solution"
    Just (SimpleSolution solution) -> do
      parsed <- view parse solution
      let part1Result = view solve1 solution <$> parsed
      let part2Result = view solve2 solution <$> parsed
      case part of
        Part1 -> print part1Result
        Part2 -> print part2Result
        Both -> do
          print part1Result
          print part2Result
