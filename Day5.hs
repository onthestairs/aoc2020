{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day5 (solution) where

import AOC (Parser, Solution (..), parseFile)
import Relude
import Relude.Extra (Foldable1 (maximum1))
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (char, newline)

data RowLocation = F | B deriving (Show)

data ColumnLocation = L | R deriving (Show)

type Input = [([RowLocation], [ColumnLocation])]

parseInput :: Parser Input
parseInput = sepBy1 parseLocation newline <* eof

parseLocation = do
  row <- parseRowLocations
  col <- parseColumnLocations
  pure (row, col)

parseRowLocation = char 'F' $> F <|> char 'B' $> B

parseRowLocations = some parseRowLocation

parseColumnLocation = char 'L' $> L <|> char 'R' $> R

parseColumnLocations = some parseColumnLocation

binaryToInt :: (a -> Int) -> [a] -> Int
binaryToInt f xs = sum $ map (\(exp, x) -> f x * (2 ^ exp)) $ zip [0 ..] (reverse xs)

findRow :: [RowLocation] -> Int
findRow ls = binaryToInt sideToCoefficient ls
  where
    sideToCoefficient F = 0
    sideToCoefficient B = 1

findColumn :: [ColumnLocation] -> Int
findColumn ls = binaryToInt sideToCoefficient ls
  where
    sideToCoefficient L = 0
    sideToCoefficient R = 1

getSeatID (rs, cs) = ((findRow rs) * 8) + findColumn cs

solve1 :: Input -> Maybe Int
solve1 ls = viaNonEmpty maximum1 $ map getSeatID ls

findMissing xs = fmap (\(i1, _) -> i1 + 1) $ find (\(i1, i2) -> i2 - i1 == 2) $ zip xs (drop 1 xs)

solve2 ls = findMissing $ sort $ map getSeatID ls

solution =
  Solution
    { _parse = parseFile "5.txt" parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
