{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day2 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt)
import Control.Lens
import Relude
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (char, lowerChar, newline, space)
import Text.Parsec.Combinator (many1)

data PasswordPolicy = PasswordPolicy
  { _policyChar :: Char,
    _policyMin :: Int,
    _policyMax :: Int
  }
  deriving (Show)

makeLenses ''PasswordPolicy

type Password = String

type Input = [(PasswordPolicy, Password)]

parseInput :: Parser Input
parseInput = sepBy1 parseLine newline <* eof

parseLine = do
  policyMin <- parseInt
  char '-'
  policyMax <- parseInt
  space
  policyChar <- lowerChar
  char ':'
  space
  password <- some lowerChar
  pure $ (PasswordPolicy {_policyMin = policyMin, _policyMax = policyMax, _policyChar = policyChar}, password)

solve1 ps = length $ filter (uncurry passwordSatisfiesPolicy) ps

passwordSatisfiesPolicy :: PasswordPolicy -> Password -> Bool
passwordSatisfiesPolicy passwordPolicy password =
  let policyChars = length $ filter (\c -> c == (view policyChar passwordPolicy)) password
   in policyChars >= (view policyMin passwordPolicy) && policyChars <= (view policyMax passwordPolicy)

solve2 ps = length $ filter (uncurry passwordSatisfiesPolicy2) ps

passwordSatisfiesPolicy2 :: PasswordPolicy -> Password -> Bool
passwordSatisfiesPolicy2 passwordPolicy password =
  let index1 = view policyMin passwordPolicy
      index2 = view policyMax passwordPolicy
      c = view policyChar passwordPolicy
      charAtIndex1 = password !!? (index1 - 1)
      charAtIndex2 = password !!? (index2 - 1)
      index1Correct = charAtIndex1 == Just c
      index2Correct = charAtIndex2 == Just c
   in case (index1Correct, index2Correct) of
        (True, False) -> True
        (False, True) -> True
        _ -> False

solution =
  Solution
    { _parse = parseFile "2.txt" parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
