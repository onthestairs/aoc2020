{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day6 (solution) where

import AOC (Parser, Solution (..), parseFile)
import qualified Data.Set as Set
import Relude
import Text.Megaparsec (anySingle, eof, lookAhead, sepBy1, try)
import Text.Megaparsec.Char (alphaNumChar, newline)

type Input = [[[Char]]]

twoNewlines = newline *> newline

parseInput :: Parser Input
parseInput = sepBy1 parseGroup twoNewlines <* eof

sepByOneNewline :: Parser x -> Parser [x]
sepByOneNewline p = do
  x <- p
  xs <- many $
    try $ do
      newline
      c <- lookAhead anySingle
      when (c == '\n') (fail "Extra newline")
      p
  pure (x : xs)

parseGroup = do
  sepByOneNewline parseAnswers

parseAnswers = some alphaNumChar

uniqueAnswersInGroup :: [[Char]] -> Set.Set Char
uniqueAnswersInGroup ps = fromList $ concat ps

solve1 groups =
  sum $ map (length . uniqueAnswersInGroup) groups

commonAnswersInGroup :: [[Char]] -> Set.Set Char
commonAnswersInGroup ps = Set.filter (\c -> all (\p -> c `elem` p) ps) uniqueAnswers
  where
    uniqueAnswers = uniqueAnswersInGroup ps

solve2 groups =
  sum $ map (length . commonAnswersInGroup) groups

solution =
  Solution
    { _parse = parseFile "6.txt" parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
