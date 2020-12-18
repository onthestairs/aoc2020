{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AOC (Solution (..), SeparateParseSolution (..), parse, solve1, solve2, parse1, parse2, solveWith1, solveWith2, GenericSolution (..), Parser, parseFile, parseInt, parseInt64, parseInteger, parseSignedInt) where

import Control.Lens
import Control.Lens.TH ()
import Relude
import Text.Megaparsec (Parsec, runParser)
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as L

data Solution a b c = Solution
  { _parse :: IO (Maybe a),
    _solve1 :: a -> b,
    _solve2 :: a -> c
  }

makeLenses ''Solution

data SeparateParseSolution a b c d = SeparateParseSolution
  { _parse1 :: IO (Maybe a),
    _solveWith1 :: a -> b,
    _parse2 :: IO (Maybe c),
    _solveWith2 :: c -> d
  }

makeLenses ''SeparateParseSolution

data GenericSolution where
  SimpleSolution :: (Show a, Show b, Show c) => Solution a b c -> GenericSolution
  TwoParseSolution :: (Show a, Show b, Show c, Show d) => SeparateParseSolution a b c d -> GenericSolution

type Path = String

type Parser = Parsec Void Text

parseFile :: Path -> Parser a -> IO (Maybe a)
parseFile path parse = do
  fileContents <- readFileText ("./data/" <> path)
  let result = runParser parse path fileContents
  pure $ rightToMaybe result

parseInt :: Parser Int
parseInt = L.decimal

parseInt64 :: Parser Int64
parseInt64 = L.decimal

parseInteger :: Parser Integer
parseInteger = L.decimal

-- parseSignedInt :: Parser Int
parseSignedInt = do
  sign <- char '+' <|> char '-'
  n <- parseInt
  let coefficient = if sign == '+' then 1 else -1
  pure $ coefficient * n