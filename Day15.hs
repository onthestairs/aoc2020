{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day15 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt)
import Control.Lens hiding (indices, (<|))
import Control.Monad.Loops (untilM_)
import qualified Data.IntMap.Strict as IntMap
import Relude hiding (Op)
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (char)

type Input = [Int]

parseInput :: Parser Input
parseInput = sepBy1 parseInt (char ',') <* eof

data GameState = GameState
  { _lastSeen :: !(IntMap.IntMap Int),
    _lastN :: !Int,
    _turnNumber :: !Int
  }

makeLenses ''GameState

turn :: State GameState ()
turn = do
  !lastN' <- use lastN
  !lastSeen' <- use lastSeen
  !turnNumber' <- use turnNumber
  let !nextN = case IntMap.lookup lastN' lastSeen' of
        Just i -> (turnNumber' - 1) - i
        Nothing -> 0
  modifying lastSeen (IntMap.insert lastN' (turnNumber' - 1))
  modifying turnNumber (+ 1)
  modifying lastN (const nextN)

findNth is n = do
  lastN' <- viaNonEmpty last is
  let initialState =
        GameState
          { _lastSeen = IntMap.fromList $ zip is [1 ..],
            _lastN = lastN',
            _turnNumber = length is + 1
          }
  pure $ view lastN $ snd $ usingState initialState (untilM_ turn ((== n + 1) . view turnNumber <$> get))

solve1 is = findNth is 2020

solve2 is = findNth is 30000000

solution =
  Solution
    { _parse = parseFile "15.txt" parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
