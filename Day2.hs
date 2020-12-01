{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day2 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt)
import Relude
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (newline)

type Input = [Int]

parseInts :: Parser Input
parseInts = sepBy1 parseInt newline <* eof

findPairsWhichSum :: [Int] -> Int -> [(Int, Int)]
findPairsWhichSum xs target =
  let pairs = [(x, y) | x <- xs, y <- xs]
   in filter (\(x, y) -> x + y == target) pairs

solve1 ns = do
  let pairs = findPairsWhichSum ns 2020
  (x, y) <- viaNonEmpty head pairs
  pure $ x * y

findTriplesWhichSum :: [Int] -> Int -> [(Int, Int, Int)]
findTriplesWhichSum xs target =
  let pairs = [(x, y, z) | x <- xs, y <- xs, z <- xs]
   in filter (\(x, y, z) -> x + y + z == target) pairs

solve2 ns = do
  let triples = findTriplesWhichSum ns 2020
  (x, y, z) <- viaNonEmpty head triples
  pure $ x * y * z

solution =
  Solution
    { _parse = parseFile "1.txt" parseInts,
      _solve1 = solve1,
      _solve2 = solve2
    }
