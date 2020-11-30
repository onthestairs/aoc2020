{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day1 (solution) where

import AOC
import Relude

solution :: Solution Int Int Text
solution =
  Solution
    { _parse = pure $ Just 5,
      _solve1 = id,
      _solve2 = const "Hello"
    }
