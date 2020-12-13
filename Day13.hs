{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day13 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInteger)
import Math.NumberTheory.Moduli (modulo)
import Math.NumberTheory.Moduli.Chinese (chineseSomeMod)
import Math.NumberTheory.Moduli.Class (SomeMod (InfMod, SomeMod), getVal)
import Relude hiding (Op)
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (char, newline)

type Bus = Maybe Integer

type Input = (Integer, [Bus])

parseInput :: Parser Input
parseInput = do
  timestamp <- parseInteger
  newline
  busses <- sepBy1 parseBus (char ',')
  eof
  pure (timestamp, busses)

parseBus = (Just <$> parseInteger) <|> (char 'x' $> Nothing)

iterateUntil p f x = if p x then x else iterateUntil p f (f x)

solve1 :: Input -> Maybe Integer
solve1 (timestamp, busses) = do
  let bs = catMaybes busses
  let bsWithDepartureTime = zip bs (map (\n -> iterateUntil (> timestamp) (+ n) n) bs)
  (bus, depatureTime) <- viaNonEmpty head $ sortOn snd bsWithDepartureTime
  pure $ bus * (depatureTime - timestamp)

solve2 (_, busses) = do
  let p b = case b of
        (o, Just n) -> Just ((n - o) `modulo` fromIntegral n)
        (_, Nothing) -> Nothing
  let bsWithOffset = mapMaybe p $ zip [0 ..] busses
  solvedMod <- foldlM chineseSomeMod (0 `modulo` 1) bsWithOffset
  case solvedMod of
    SomeMod k -> Just (getVal k)
    InfMod {} -> Nothing

solution =
  Solution
    { _parse = parseFile "13.txt" parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
