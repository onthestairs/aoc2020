{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AOC (Solution (..), parse, solve1, solve2, GenericSolution (..)) where

import Control.Lens
import Control.Lens.TH ()
import Relude
import Text.Megaparsec (Parsec, runParser)

data Solution a b c = Solution
  { _parse :: IO (Maybe a),
    _solve1 :: a -> b,
    _solve2 :: a -> c
  }

makeLenses ''Solution

data GenericSolution where
  SimpleSolution :: (Show a, Show b, Show c) => Solution a b c -> GenericSolution

type Path = String

type Parser = Parsec Void Text

parseFile :: Path -> Parser a -> IO (Maybe a)
parseFile path parse = do
  fileContents <- readFileText path
  let result = runParser parse path fileContents
  pure $ rightToMaybe result