{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day8 (solution) where

import AOC (Parser, Solution (..), parseFile, parseSignedInt)
import Control.Lens hiding (contains)
import qualified Data.Set as Set
import Relude hiding (Op)
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (newline, string)

data Op = Nop Int | Acc Int | Jmp Int deriving (Show)

type Input = [Op]

parseInput :: Parser Input
parseInput = sepBy1 parseOp newline <* eof

parseOp = do
  pNop <|> pAcc <|> pJmp
  where
    pNop = Nop <$> (string "nop " *> parseSignedInt)
    pAcc = Acc <$> (string "acc " *> parseSignedInt)
    pJmp = Jmp <$> (string "jmp " *> parseSignedInt)

data BootCodeState = BootCodeState
  { _acc :: Int,
    _cursor :: Int,
    _visitedOps :: Set.Set Int,
    _infiniteLoopEntered :: Bool
  }
  deriving (Show)

makeLenses ''BootCodeState

runOp :: Op -> State BootCodeState ()
runOp (Nop _) = modifying cursor (+ 1)
runOp (Acc n) = do
  modifying acc (+ n)
  modifying cursor (+ 1)
runOp (Jmp n) = modifying cursor (+ n)

runBootCode :: BootCodeState -> [Op] -> BootCodeState
runBootCode s ops = snd $ usingState s go
  where
    go :: State BootCodeState ()
    go = do
      cursor' <- use cursor
      visitedOps' <- use visitedOps
      if Set.member cursor' visitedOps'
        then assign infiniteLoopEntered True
        else do
          case ops !!? cursor' of
            Just op -> do
              modifying visitedOps (Set.insert cursor')
              runOp op
              go
            Nothing -> do
              pure ()

runToCompletion ops = runBootCode initialState ops
  where
    initialState =
      BootCodeState
        { _acc = 0,
          _cursor = 0,
          _visitedOps = Set.empty,
          _infiniteLoopEntered = False
        }

solve1 ops = view acc $ runToCompletion ops

makeOpSwaps :: [Op] -> [[Op]]
makeOpSwaps ops = go ops
  where
    go (Jmp n : rest) = (Nop n : rest) : [(Jmp n : others) | others <- go rest]
    go (Nop n : rest) = (Jmp n : rest) : [(Nop n : others) | others <- go rest]
    go (op : rest) = [op : others | others <- go rest]
    go [] = [[]]

-- solve2 :: [Op] -> Maybe Int
solve2 ops = view acc <$> find (not . view infiniteLoopEntered) (map runToCompletion swaps)
  where
    swaps = makeOpSwaps ops

solution =
  Solution
    { _parse = parseFile "8.txt" parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
