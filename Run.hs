{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run where

import AOC
import Control.Lens
import qualified Data.Map.Strict as Map
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Relude

solutions =
  Map.fromList
    [ (1, SimpleSolution Day1.solution),
      (2, SimpleSolution Day2.solution),
      (3, SimpleSolution Day3.solution),
      (4, SimpleSolution Day4.solution),
      (5, SimpleSolution Day5.solution),
      (6, SimpleSolution Day6.solution),
      (7, SimpleSolution Day7.solution)
    ]

data Part = Part1 | Part2 | Both

peekInput :: Int -> IO ()
peekInput day = do
  let maybeSolution = Map.lookup day solutions
  case maybeSolution of
    Nothing -> putStrLn "No solution"
    Just (SimpleSolution solution) -> do
      parsed <- view parse solution
      print parsed

showSolution (SimpleSolution solution) part = do
  maybeParsed <- view parse solution
  case maybeParsed of
    Nothing -> putStrLn "Couldn't parse"
    Just parsed -> do
      let part1Result = view solve1 solution $ parsed
      let part2Result = view solve2 solution $ parsed
      case part of
        Part1 -> print part1Result
        Part2 -> print part2Result
        Both -> do
          print part1Result
          print part2Result

solve :: Int -> Part -> IO ()
solve day part = do
  let maybeSolution = Map.lookup day solutions
  case maybeSolution of
    Nothing -> putStrLn "No solution"
    Just solution -> showSolution solution part

solveAll :: IO ()
solveAll = do
  let solutionsList = Map.toList solutions
  for_ solutionsList \(day, solution) -> do
    putStrLn $ "Day " <> show day
    putStrLn "------"
    showSolution solution Both
    putStrLn ""