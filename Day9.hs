{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day9 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt)
import Relude hiding (Op)
import Relude.Extra (Foldable1 (minimum1), maximum1)
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (newline)

type Input = [Int]

parseInput :: Parser Input
parseInput = sepBy1 parseInt newline <* eof

windows n xs = case splitAt n xs of
  (y : ys, allZs@(z : _)) -> (z, y : ys) : windows n (ys <> allZs)
  (ys, [z]) -> [(z, ys)]
  _ -> []

solve1 xs = fst <$> find (\(x, ys) -> not $ any (\(y1, y2) -> y1 + y2 == x) [(y1, y2) | y1 <- ys, y2 <- ys]) pairs
  where
    pairs = windows 25 xs

findPrefixSum target (x : xs') = case x `compare` target of
  LT -> (x :) <$> findPrefixSum (target - x) xs'
  EQ -> Just [x]
  GT -> Nothing
findPrefixSum _ [] = Nothing

findContiguousSet target (x : xs) = case findPrefixSum target xs of
  Just prefix -> Just prefix
  Nothing -> findContiguousSet target xs
findContiguousSet _ [] = Nothing

solve2 xs = do
  target <- solve1 xs
  ys <- findContiguousSet target (takeWhile (/= target) xs)
  y1 <- viaNonEmpty minimum1 ys
  y2 <- viaNonEmpty maximum1 ys
  pure $ y1 + y2

solution =
  Solution
    { _parse = parseFile "9.txt" parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
