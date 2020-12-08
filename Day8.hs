{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day8 (solution) where

import AOC (Parser, Solution (..), parseFile, parseSignedInt)
import Control.Lens hiding (contains)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Relude hiding (Op)
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (alphaNumChar, char, newline, string)

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
    _visitedOps :: Set.Set Int
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
      unless (Set.member cursor' visitedOps') $ do
        let op = fromMaybe (error "out of range") (ops !!? cursor')
        modifying visitedOps (Set.insert cursor')
        runOp op
        go

solve1 ops = view acc $ runBootCode initialState ops
  where
    initialState =
      BootCodeState
        { _acc = 0,
          _cursor = 0,
          _visitedOps = Set.empty
        }

solution =
  Solution
    { _parse = parseFile "8.txt" parseInput,
      _solve1 = solve1,
      _solve2 = const 2
    }
