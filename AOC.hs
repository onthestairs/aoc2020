{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AOC (Solution (..), parse, solve1, solve2, GenericSolution (..), Parser, parseFile, parseInt, parseInt64, parseInteger, parseSignedInt) where

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

data GenericSolution where
  SimpleSolution :: (Show a, Show b, Show c) => Solution a b c -> GenericSolution

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