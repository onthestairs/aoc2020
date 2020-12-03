{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day3 (solution) where

import AOC (Parser, Solution (..), parseFile)
import Control.Lens
import Relude
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (char, newline)

data Cell = Space | Tree deriving (Show, Eq)

type Input = [[Cell]]

parseInput :: Parser Input
parseInput = sepBy1 parseLine newline <* eof

parseCell :: Parser Cell
parseCell = (char '.' $> Space) <|> (char '#' $> Tree)

parseLine :: Parser [Cell]
parseLine = some parseCell

data Grid = Grid
  { _getCell :: (Int, Int) -> Maybe Cell
  }

makeLenses ''Grid

visitGridSlope :: Grid -> Int -> Int -> [Cell]
visitGridSlope grid dx dy =
  let coords = iterate (\(x, y) -> (x + dx, y + dy)) (0, 0)
   in catMaybes $ takeWhile isJust $ map (view getCell grid) coords

makeGrid cells =
  let maybeWidth = length <$> cells !!? 0
   in Grid
        { _getCell =
            \(x, y) -> do
              width <- maybeWidth
              row <- cells !!? y
              row !!? (x `mod` width)
        }

numberOfTreesVisited grid dx dy =
  let visitedCells = visitGridSlope grid dx dy
   in length $ filter (Tree ==) visitedCells

solve1 cells =
  let grid = makeGrid cells
   in numberOfTreesVisited grid 3 1

solve2 cells =
  let grid = makeGrid cells
      slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
      treesSeen = map (\(dx, dy) -> numberOfTreesVisited grid dx dy) slopes
   in product treesSeen

solution =
  Solution
    { _parse = parseFile "3.txt" parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
