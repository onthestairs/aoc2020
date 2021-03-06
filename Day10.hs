{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day10 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt)
import qualified Data.Map.Strict as Map
import qualified Data.MultiSet as Set
import qualified Data.Sequence as Seq
import Relude hiding (Op)
import Relude.Extra (maximum1)
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (newline)

type Input = [Int]

parseInput :: Parser Input
parseInput = sepBy1 parseInt newline <* eof

differences xs = zipWith (-) (drop 1 xs) xs

addToEnds xs = do
  x <- viaNonEmpty maximum1 xs
  pure $ 0 : (x + 3) : xs

count = Set.fromList

solve1 xs = do
  ys <- addToEnds xs
  let counts = (count . differences . sort) ys
  pure $ (Set.occur 1 counts) * (Set.occur 3 counts)

findNumberOfLegalPaths :: Seq.Seq Int -> Int
findNumberOfLegalPaths ys = fst $ usingState Map.empty (go 0)
  where
    go :: Int -> State (Map.Map Int Int) Int
    go i = do
      cache <- get
      case Map.lookup i cache of
        Just n -> pure n
        Nothing -> do
          if i == length ys - 1 -- final number has been reached
            then pure 1
            else do
              let current = fromMaybe (error "bad cursor") $ Seq.lookup i ys
              let nextIs = takeWhile (isValidNext current) [i + 1 ..]
              ns <- forM nextIs $ \i -> do
                n <- go i
                modify $ Map.insert i n
                pure n
              pure $ sum ns
    isValidNext current j = case Seq.lookup j ys of
      Just j -> j - current <= 3
      Nothing -> False

solve2 xs = do
  ys <- addToEnds xs
  pure $ findNumberOfLegalPaths $ Seq.fromList (sort ys)

solution =
  Solution
    { _parse = parseFile "10.txt" parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
