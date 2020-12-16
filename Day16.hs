{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day16 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt)
import Data.List ((\\))
import Relude hiding (Op)
import Text.Megaparsec (anySingle, eof, lookAhead, sepBy1, try)
import Text.Megaparsec.Char (alphaNumChar, char, newline, string)

type Rule = (String, (Int, Int), (Int, Int))

type Ticket = [Int]

type Input = ([Rule], Ticket, [Ticket])

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

-- parseInput :: Parser Input
parseInput = do
  rules <- parseRules
  newline
  newline
  string "your ticket:"
  newline
  myTicket <- parseTicket
  newline
  newline
  string "nearby tickets:"
  newline
  tickets <- sepBy1 parseTicket newline
  pure (rules, myTicket, tickets)

parseRules = sepByOneNewline parseRule

parseRule = do
  name <- many (alphaNumChar <|> char ' ')
  string ": "
  range1 <- parseRange
  string " or "
  range2 <- parseRange
  pure (name, range1, range2)

parseRange = do
  i1 <- parseInt
  char '-'
  i2 <- parseInt
  pure (i1, i2)

parseTicket = sepBy1 parseInt (char ',')

isInRange (l, u) n = n >= l && n <= u

satisfiesRule (_, r1, r2) n = isInRange r1 n || isInRange r2 n

isPossiblyValid rules n = any (\rule -> satisfiesRule rule n) rules

findInvalidValues rules t = filter (\n -> not $ isPossiblyValid rules n) t

solve1 (rules, _, ts) = sum $ concatMap (findInvalidValues rules) ts

startsWith prefix s = take (length prefix) s == prefix

findPossibleRules rules vs = filter (\rule -> all (\v -> satisfiesRule rule v) vs) rules

findTicketOrderings :: [Rule] -> [[Int]] -> [[Rule]]
findTicketOrderings rules ts = go rules (transpose ts)
  where
    go :: [Rule] -> [[Int]] -> [[Rule]]
    go rules' [vs] = map (\x -> [x]) (findPossibleRules rules' vs)
    go rules' (vs : vss) =
      [rule : rest | rule <- findPossibleRules rules' vs, rest <- go (rules' \\ [rule]) vss]

fst3 (a, _, _) = a

solve2 :: Input -> Maybe Int
solve2 (rules, t, ts) = do
  let validTickets = filter (null . findInvalidValues rules) ts
  let ticketOrderings = findTicketOrderings rules (t : validTickets)
  ticketOrdering <- viaNonEmpty head ticketOrderings
  let myTicketLabelled = zip (map fst3 ticketOrdering) t
  pure $ product $ map snd $ filter (startsWith "departure" . fst) myTicketLabelled

solution =
  Solution
    { _parse = parseFile "16.txt" parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
