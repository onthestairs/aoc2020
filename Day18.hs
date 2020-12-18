{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day18 (solution) where

import AOC (Parser, SeparateParseSolution (..), parseFile, parseInt)
import Control.Monad.Combinators.Expr
import Relude hiding (Op, Product, Sum)
import Text.Megaparsec (MonadParsec (try), between, choice, eof, parseTest, sepBy1)
import Text.Megaparsec.Char (char, newline, string)
import qualified Text.Megaparsec.Char.Lexer as L

data Expr
  = Int Int
  | Sum Expr Expr
  | Product Expr Expr
  deriving (Eq, Ord, Show)

makeExpressionParser operatorTable = pExpr
  where
    pExpr = makeExprParser pTerm operatorTable
    pTerm :: Parser Expr
    pTerm =
      choice
        [ parens pExpr,
          pInt
        ]

pInt = Int <$> parseInt

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

parseInput operatorTable = sepBy1 (makeExpressionParser operatorTable) newline

operatorTable1 :: [[Operator Parser Expr]]
operatorTable1 =
  [ [ binaryL "+" Sum,
      binaryL "*" Product
    ]
  ]

binaryL :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binaryL name f = InfixL (f <$ string (" " <> name <> " "))

parseInput1 = parseInput operatorTable1

eval :: Expr -> Int
eval (Int n) = n
eval (Sum e1 e2) = eval e1 + eval e2
eval (Product e1 e2) = eval e1 * eval e2

solve es = sum $ map eval es

operatorTable2 :: [[Operator Parser Expr]]
operatorTable2 =
  [ [ binary "+" Sum
    ],
    [ binary "*" Product
    ]
  ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ string (" " <> name <> " "))

parseInput2 = parseInput operatorTable2

solution =
  SeparateParseSolution
    { _parse1 = parseFile "18.txt" parseInput1,
      _solveWith1 = solve,
      _parse2 = parseFile "18.txt" parseInput2,
      _solveWith2 = solve
    }
