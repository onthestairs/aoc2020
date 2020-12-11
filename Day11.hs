{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day11 (solution) where

import AOC (Parser, Solution (..), parseFile)
import qualified Data.Vector as Vec
import Relude hiding (Op)
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (char, newline)

data Status = E | O | F | OffGrid deriving (Show, Eq)

type Input = [[Status]]

parseInput :: Parser Input
parseInput = sepBy1 (many $ (char '.' $> F) <|> (char 'L' $> E)) newline <* eof

makeMap cells = Vec.fromList $ map Vec.fromList cells

rule E neigbours = if O `notElem` neigbours then O else E
rule O neigbours = if length (filter (O ==) neigbours) >= 4 then E else O
rule s _ = s

flatten :: Vec.Vector (Vec.Vector a) -> Vec.Vector a
flatten xss = Vec.concat $ Vec.toList xss

getNeighbours (row, col) m = [getCellValue coords' m | coords' <- allNeighbours]
  where
    allNeighbours = [(row + i, col + j) | i <- [-1, 0, 1], j <- [-1, 0, 1], (i, j) /= (0, 0)]

getCellValue :: (Int, Int) -> Vec.Vector (Vec.Vector Status) -> Status
getCellValue (rowIndex, colIndex) grid =
  fromMaybe OffGrid $
    do
      row <- (Vec.!?) grid rowIndex
      (Vec.!?) row colIndex

makeNew f m = Vec.imap (\rowIndex row -> Vec.imap (\colIndex v -> f (rowIndex, colIndex) v) row) m

step m = makeNew (\coords v -> rule v (getNeighbours coords m)) m

iterateUntil p f x = if p x then x else iterateUntil p f (f x)

converge f x = do
  let ss = iterate f x
  let pairs = dropWhile (uncurry (/=)) (zip ss (drop 1 ss))
  fixedPointPair <- viaNonEmpty head pairs
  pure $ fst fixedPointPair

solve1 cells = do
  let m = makeMap cells
  finalState <- converge step m
  pure $ length $ filter (== O) (toList $ flatten finalState)

getVisible (row, col) m = map findFirstVisible deltas
  where
    deltas = [(i, j) | i <- [-1, 0, 1], j <- [-1, 0, 1], (i, j) /= (0, 0)]
    findFirstVisible (dRow, dCol) = (\coords -> getCellValue coords m) $ iterateUntil (\coords' -> (getCellValue coords' m) /= F) (\(row', col') -> (row' + dRow, col' + dCol)) (row + dRow, col + dCol)

rule2 E visible = if O `notElem` visible then O else E
rule2 O visible = if length (filter (O ==) visible) >= 5 then E else O
rule2 s _ = s

step2 m = makeNew (\coords v -> rule2 v (getVisible coords m)) m

solve2 cells = do
  let m = makeMap cells
  finalState <- converge step2 m
  pure $ length $ filter (== O) (toList $ flatten finalState)

solution =
  Solution
    { _parse = parseFile "11.txt" parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
