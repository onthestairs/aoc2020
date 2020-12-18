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

data Status = Active | Inactive deriving Show

rule Active nActive  = if nActive == 2 || nActive == 3 then Active else Inactive
rule Inactive nActive  = if nActive == 3  then Active else Inactive

minimumOn p = minimumBy (compare `on` p)
maximumOn p = maximumBy (compare `on` p)

iterateN 0 _ x = x
iterateN n f x = iterateN (n-1) f (f x)

makeGrid3 is = Set.fromList $ [(col, row, 0) | (row, ys) <- zip [0 ..] is, (col, x) <- zip [0 ..] ys, x == '#']

deltas3  = [(x, y, z) | x <- [-1,0,1], y<-[-1,0,1], z<-[-1,-0,1], (x,y,z) /= (0,0,0)]
neighbours3 (x,y,z) = [(x+dx, y+dy, z+dz) | (dx,dy,dz) <- deltas3]

fst3 (x,_,_) = x
snd3 (_,y,_) = y
thd3 (_,_,z) = z

getBounds3 g = ((x1,y1,z1), (x2,y2,z2))
  where
    x1 = fst3 (minimumOn fst3 g) - 1
    x2 = fst3 (maximumOn fst3 g) + 1
    y1 = snd3 (minimumOn snd3 g) - 1
    y2 = snd3 (maximumOn snd3 g) + 1
    z1 = thd3 (minimumOn thd3 g) - 1
    z2 = thd3 (maximumOn thd3 g) + 1

step3 g = foldl' f g range
  where
    f g' c = case rule cStatus activeNeighbours of
              Active -> Set.insert c g'
              Inactive ->  Set.delete c g'
      where
        cStatus = if Set.member c g then Active else Inactive
        activeNeighbours = activeCubes (neighbours3 c)
    activeCubes cs = length $ [c| c <- cs,  Set.member c g]
    ((x1,y1,z1), (x2,y2,z2)) = getBounds3 g
    range = [(x,y,z) | x <- [x1..x2], y <- [y1..y2], z <- [z1..z2]]


solve1 i = Set.size $ iterateN 6 step3 m
  where
    m = makeGrid3 i

makeGrid4 is = Set.fromList $ [(col, row, 0, 0) | (row, ys) <- zip [0 ..] is, (col, x) <- zip [0 ..] ys, x == '#']

deltas4  = [(x, y, z, w) | x <- [-1,0,1], y<-[-1,0,1], z<-[-1,-0,1], w<-[-1,0,1], (x,y,z,w) /= (0,0,0,0)]
neighbours4 (x,y,z,w) = [(x+dx, y+dy, z+dz, w+dw) | (dx,dy,dz,dw) <- deltas4]

fst4 (x,_,_,_) = x
snd4 (_,y,_,_) = y
thd4 (_,_,z,_) = z
fth4 (_,_,_,w) = w

getBounds4 g = ((x1,y1,z1,w1), (x2,y2,z2,w2))
  where
    x1 = fst4 (minimumOn fst4 g) - 1
    x2 = fst4 (maximumOn fst4 g) + 1
    y1 = snd4 (minimumOn snd4 g) - 1
    y2 = snd4 (maximumOn snd4 g) + 1
    z1 = thd4 (minimumOn thd4 g) - 1
    z2 = thd4 (maximumOn thd4 g) + 1
    w1 = fth4 (minimumOn fth4 g) - 1
    w2 = fth4 (maximumOn fth4 g) + 1

step4 g = foldl' f g range
  where
    f g' c = case rule cStatus activeNeighbours of
              Active -> Set.insert c g'
              Inactive ->  Set.delete c g'
      where
        cStatus = if Set.member c g then Active else Inactive
        activeNeighbours = activeCubes (neighbours4 c)
    activeCubes cs = length $ [c| c <- cs,  Set.member c g]
    ((x1,y1,z1,w1), (x2,y2,z2,w2)) = getBounds4 g
    range = [(x,y,z,w) | x <- [x1..x2], y <- [y1..y2], z <- [z1..z2], w <-[w1..w2]]

solve2 i = Set.size $ iterateN 6 step4 m
  where
    m = makeGrid4 i

solution =
  Solution
    { _parse = parseFile "17.txt" parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
