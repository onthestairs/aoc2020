{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day14 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt64)
import Control.Lens
import Data.Bits (Bits (setBit), clearBit)
import qualified Data.Map.Strict as Map
import Relude hiding (Op)
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (char, newline, string)

data MaskBit = Floating | Zero | One deriving (Show)

type Mask = [MaskBit]

data Op = SetMask Mask | WriteToMemory Int64 Int64 deriving (Show)

type Input = [Op]

parseInput :: Parser Input
parseInput = sepBy1 parseOp newline <* eof

parseOp = parseMask <|> parseWriteMem

parseMask = do
  string "mask = "
  bits <- some (char 'X' $> Floating <|> char '0' $> Zero <|> char '1' $> One)
  pure $ SetMask bits

parseWriteMem = do
  string "mem["
  address <- parseInt64
  string "] = "
  value <- parseInt64
  pure $ WriteToMemory address value

data PortState = PortState
  { _mask :: Mask,
    _memory :: Map.Map Int64 Int64
  }

makeLenses ''PortState

applyMask :: Mask -> Int64 -> Int64
applyMask m v = foldl' setBitFromMask v (zip [0 ..] (reverse m))
  where
    setBitFromMask v' (i, Zero) = clearBit v' i
    setBitFromMask v' (i, One) = setBit v' i
    setBitFromMask v' _ = v'

runOp (SetMask m) = modifying mask (const m)
runOp (WriteToMemory address val) = do
  currentMask <- use mask
  modifying memory (Map.insert address (applyMask currentMask val))

runOps ops = mapM_ runOp ops

solve1 ops = sum $ Map.elems $ view memory $ snd $ usingState initialState (runOps ops)
  where
    initialState =
      PortState
        { _mask = [],
          _memory = Map.empty
        }

calculateAddresses :: Mask -> Int64 -> [Int64]
calculateAddresses m a = go (zip [0 ..] (reverse m)) a
  where
    go ((_, Zero) : rest) a' = go rest a'
    go ((i, One) : rest) a' = go rest (setBit a' i)
    go ((i, Floating) : rest) a' = go rest (setBit a' i) ++ go rest (clearBit a' i)
    go _ a' = [a']

runOp2 (SetMask m) = modifying mask (const m)
runOp2 (WriteToMemory address val) = do
  currentMask <- use mask
  let addresses = calculateAddresses currentMask address
  modifying memory (\mem -> foldl' (\mem' address' -> Map.insert address' val mem') mem addresses)

runOps2 ops = mapM_ runOp2 ops

solve2 ops = sum $ Map.elems $ view memory $ snd $ usingState initialState (runOps2 ops)
  where
    initialState =
      PortState
        { _mask = [],
          _memory = Map.empty
        }

solution =
  Solution
    { _parse = parseFile "14.txt" parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
