{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day11 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt)
import qualified Data.Map.Strict as Map
import qualified Data.MultiSet as Set
import qualified Data.Sequence as Seq
import Relude hiding (Op)
import Relude.Extra (maximum1)
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (char, newline)

data Status = E | O | F | OffGrid deriving (Show, Eq)

type Input = [[Status]]

parseInput :: Parser Input
parseInput = sepBy1 (many $ (char '.' $> F) <|> (char 'L' $> E)) newline <* eof

makeMap cells = Map.fromList [((row, col), value) | (row, rowCells) <- zip [0 ..] cells, (col, value) <- zip [0 ..] rowCells]

rule E neigbours = if O `notElem` neigbours then O else E
rule O neigbours = if length (filter (O ==) neigbours) >= 4 then E else O
rule s _ = s

getNeighbours (row, col) m = [Map.findWithDefault OffGrid coords' m | coords' <- allNeighbours]
  where
    allNeighbours = [(row + i, col + j) | i <- [-1, 0, 1], j <- [-1, 0, 1], (i, j) /= (0, 0)]

step m = Map.mapWithKey (\coords v -> rule v (getNeighbours coords m)) m

showGrid (height, width) m = unlines $ map toText $ [[showChar (Map.findWithDefault OffGrid (row, col) m) | col <- [0 .. width -1]] | row <- [0 .. height -1]]
  where
    showChar E = 'L'
    showChar O = '#'
    showChar F = '.'

iterateUntil p f x = if p x then x else iterateUntil p f (f x)

converge f x = do
  let ss = iterate f x
  let pairs = dropWhile (uncurry (/=)) (zip ss (drop 1 ss))
  fixedPointPair <- viaNonEmpty head pairs
  pure $ fst fixedPointPair

solve1 cells = do
  let m = makeMap cells
  finalState <- converge step m
  pure $ length $ filter (== O) (Map.elems finalState)

getVisible :: (Int, Int) -> Map.Map (Int, Int) Status -> [Status]
getVisible (row, col) m = map findFirstVisible deltas
  where
    deltas = [(i, j) | i <- [-1, 0, 1], j <- [-1, 0, 1], (i, j) /= (0, 0)]
    getCell coords = Map.findWithDefault OffGrid coords m
    findFirstVisible (dRow, dCol) = getCell $ iterateUntil (\coords' -> (getCell coords') /= F) (\(row', col') -> (row' + dRow, col' + dCol)) (row + dRow, col + dCol)

rule2 E visible = if O `notElem` visible then O else E
rule2 O visible = if length (filter (O ==) visible) >= 5 then E else O
rule2 s _ = s

step2 m = Map.mapWithKey (\coords v -> rule2 v (getVisible coords m)) m

solve2 cells = do
  let m = makeMap cells
  finalState <- converge step2 m
  pure $ length $ filter (== O) (Map.elems finalState)

solution =
  Solution
    { _parse = parseFile "11.txt" parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
