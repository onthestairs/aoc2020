{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day15 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt)
import Control.Lens hiding (indices, (<|))
import Data.Bits (Bits (setBit), clearBit)
import qualified Data.IntMap as IntMap
import Data.List.NonEmpty ((<|))
import qualified Data.Map.Strict as Map
import Relude hiding (Op)
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (char, newline, string)

type Input = [Int]

parseInput :: Parser Input
parseInput = sepBy1 parseInt (char ',') <* eof

data GameState = GameState
  { _indices :: IntMap.IntMap (NonEmpty Int),
    _history :: (NonEmpty Int),
    _turnNumber :: Int
    -- _n :: Int
    -- _currentIndex :: Int
  }

makeLenses ''GameState

turn :: State GameState ()
turn = do
  turnHistory <- use history
  let lastN = head turnHistory
  indicesHistory <- use indices
  turnNumber' <- use turnNumber
  let turnNumber = length turnHistory + 1
  let nextN = case IntMap.lookup lastN indicesHistory of
        Just (i :| (i' : _)) -> i - i'
        Just (i :| []) -> 0
        Nothing -> 0
  modifying history (nextN <|)
  modifying indices (IntMap.insertWith (<>) nextN (turnNumber' :| []))
  modifying turnNumber (+ 1)

turnUntil p = do
  turn
  s <- get
  if p s
    then pure ()
    else turnUntil p

findNth is n = head $ view history $ snd $ usingState initialState (turnUntil ((== n) . length . view history))
  where
    initialState =
      GameState
        { _indices = IntMap.fromList $ zip is (map (:| []) [1 ..]),
          _history = fromList (reverse is)
          -- _n =
        }

solve1 is = findNth is 2020

solve2 is = findNth is 30000000

solution =
  Solution
    { _parse = parseFile "15.txt" parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
