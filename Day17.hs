{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day17 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt)
import Data.List ((\\), minimumBy, maximumBy, minimumBy)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Relude hiding (Op)
import Text.Megaparsec (anySingle, eof, lookAhead, sepBy1, try)
import Text.Megaparsec.Char (alphaNumChar, char, newline, string)

type Input = [[Char]]

parseInput = sepBy1 parseRow newline

parseRow = many (char '#' <|> char '.')

makeGrid is = Set.fromList $ [(col, row, 0) | (row, ys) <- zip [0 ..] is, (col, x) <- zip [0 ..] ys, x == '#']


deltas  = [(x, y, z) | x <- [-1,0,1], y<-[-1,0,1], z<-[-1,-0,1], (x,y,z) /= (0,0,0)]
neighbours (x,y,z) = [(x+dx, y+dy, z+dz) | (dx,dy,dz) <- deltas]

data Status = Active | Inactive deriving Show

rule Active nActive  = if nActive == 2 || nActive == 3 then Active else Inactive
rule Inactive nActive  = if nActive == 3  then Active else Inactive

minimumOn p = minimumBy (compare `on` p)
maximumOn p = maximumBy (compare `on` p)

fst3 (x,_,_) = x
snd3 (_,y,_) = y
thd3 (_,_,z) = z

getBounds g = ((x1,y1,z1), (x2,y2,z2))
  where
    x1 = (fst3 $ minimumOn fst3 g) - 1
    x2 = (fst3 $ maximumOn fst3 g) + 1
    y1 = (snd3 $ minimumOn snd3 g) - 1
    y2 = (snd3 $ maximumOn snd3 g) + 1
    z1 = (thd3 $ minimumOn thd3 g) - 1
    z2 = (thd3 $ maximumOn thd3 g) + 1

step :: Set.Set (Int,Int,Int) -> Set.Set (Int,Int,Int)
step g = foldl' f (traceShowId g) (range)
  where
    f g' c = case rule cStatus activeNeighbours of
              Active -> Set.insert c g'
              Inactive ->  Set.delete c g'
              -- Active -> (traceShow (c, cStatus, activeNeighbours, Active) $ Set.insert c g')
              -- Inactive -> (traceShow (c, cStatus, activeNeighbours, Inactive) $ Set.delete c g')
      where
        cStatus = if Set.member c g then Active else Inactive
        activeNeighbours = activeCubes (neighbours c)
    activeCubes cs = length $ [c| c <- cs,  Set.member c g]
    ((x1,y1,z1), (x2,y2,z2)) = traceShowId $ getBounds g
    range = [(x,y,z) | x <- [x1..x2], y <- [y1..y2], z <- [z1..z2]]

iterateN 0 _ x = x
iterateN n f x = iterateN (n-1) f (f x)

solve1 i = Set.size $ iterateN 6 step m
  where
    m = makeGrid i

solution =
  Solution
    { _parse = parseFile "17.txt" parseInput,
      _solve1 = solve1,
      _solve2 = const 2
    }
