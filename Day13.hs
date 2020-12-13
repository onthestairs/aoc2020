{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day13 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInteger)
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

-- taken from https://stackoverflow.com/a/35529381
crt :: (Integral a, Foldable t) => t (a, a) -> (a, a)
crt = foldr go (0, 1)
  where
    go (r1, m1) (r2, m2) = (r `mod` m, m)
      where
        r = r2 + m2 * (r1 - r2) * (m2 `inv` m1)
        m = m2 * m1

    -- Modular Inverse
    a `inv` m = let (_, i, _) = gcd a m in i `mod` m

    -- Extended Euclidean Algorithm
    gcd 0 b = (b, 0, 1)
    gcd a b = (g, t - (b `div` a) * s, s)
      where
        (g, s, t) = gcd (b `mod` a) a

---
solve2 (_, busses) = b - a
  where
    bsWithOffset = mapMaybe p $ zip [0 ..] busses
    p (o, Just n) = Just (o, n)
    p (_, Nothing) = Nothing
    (a, b) = crt bsWithOffset

solution =
  Solution
    { _parse = parseFile "13.txt" parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
