{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day18 (solution) where

import AOC (Parser, SeparateParseSolution (..), parseFile, parseInt)
import Control.Monad.Combinators.Expr
import Relude hiding (Op, Product, Sum)
import Text.Megaparsec (MonadParsec (try), between, choice, eof, parseTest, sepBy1)
import Text.Megaparsec.Char (char, newline, string)
import qualified Text.Megaparsec.Char.Lexer as L

-- import Text.Megaparsec.Char.Lexer (symbol)

data Op = Plus | Times deriving (Show)

data Exp = Exp Exp Op Exp | BracketedExp Exp | Num Int deriving (Show)

parseInts :: Parser [Exp]
parseInts = sepBy1 parseExp newline <* eof

parseExp = (try parseNormalExp) <|> (parseBracketedExp) <|> parseNum

parseBracketedExp = do
  char '('
  exp <- parseExp
  char ')'
  pure $ BracketedExp exp

parseNum = Num <$> parseInt

parseNormalExp = do
  exp1 <- (parseBracketedExp <|> parseNum)
  char ' '
  op <- parseOp
  char ' '
  exp <- parseExp
  pure $ Exp exp1 op exp

parseOp = char '+' $> Plus <|> char '*' $> Times

eval :: Exp -> Int
eval (Num n) = n
eval (BracketedExp e) = eval e
eval (Exp e1 op (Num n)) = case op of
  Plus -> eval e1 + n
  Times -> eval e1 * n
eval (Exp e1 op (BracketedExp e)) = case op of
  Plus -> eval e1 + eval e
  Times -> eval e1 * eval e
eval (Exp e1 op (Exp e2 op2 e3)) = case op of
  Plus -> eval $ Exp (Num $ eval e1 + eval e2) op2 e3
  Times -> eval $ Exp (Num $ eval e1 * eval e2) op2 e3

solve1 es = sum . map eval $ es

data Expr
  = Int Int
  | Sum Expr Expr
  | Product Expr Expr
  deriving (Eq, Ord, Show)

pExpr :: Parser Expr
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

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ binary "+" Sum
    ],
    [ binary "*" Product
    ]
  ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ string (" " <> name <> " "))

eval2 :: Expr -> Int
eval2 (Int n) = n
eval2 (Sum e1 e2) = eval2 e1 + eval2 e2
eval2 (Product e1 e2) = eval2 e1 * eval2 e2

solve2 es = sum $ map eval2 es

parseInput2 = sepBy1 pExpr newline

solution =
  SeparateParseSolution
    { _parse1 = parseFile "18.txt" parseInts,
      _solveWith1 = solve1,
      _parse2 = parseFile "18.txt" parseInput2,
      _solveWith2 = solve2
    }
