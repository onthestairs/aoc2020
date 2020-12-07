{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day7 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt)
import Control.Lens hiding (contains)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Relude
import Text.Megaparsec (anySingle, eof, lookAhead, sepBy1, try)
import Text.Megaparsec.Char (alphaNumChar, char, newline, string)

data BagRule = BagRule
  { _colour :: String,
    _contains :: [(Int, String)]
  }
  deriving (Show)

makeLenses ''BagRule

type Input = [BagRule]

parseInput :: Parser Input
parseInput = sepBy1 parseBagRule newline <* eof

parseColour = do
  adjective <- some alphaNumChar
  char ' '
  colour <- some alphaNumChar
  pure $ adjective <> " " <> colour

parseNumberOfBags = do
  n <- parseInt
  char ' '
  colour <- parseColour
  char ' '
  string "bag"
  when (n > 1) (char 's' $> ())
  pure $ (n, colour)

parseBagRule = do
  colour <- parseColour
  string " bags contain "
  contains <- (string "no other bags" $> []) <|> sepBy1 parseNumberOfBags (string ", ")
  char '.'
  pure $
    BagRule
      { _colour = colour,
        _contains = contains
      }

makeGraph :: Input -> Map.Map String [(Int, String)]
makeGraph rules = Map.fromList $ map (\br -> (view colour br, view contains br)) rules

findHeldBags :: Map.Map String [(Int, String)] -> String -> State (Map.Map String (Set.Set String)) (Set.Set String)
findHeldBags rules colour = do
  cache <- get
  case Map.lookup colour cache of
    Just c -> pure c
    Nothing -> do
      allHeldBags <- case Map.lookup colour rules of
        Just cs -> foldlM (\heldBags (_, heldColour) -> Set.union (Set.union heldBags (Set.singleton heldColour)) <$> findHeldBags rules heldColour) Set.empty cs
        Nothing -> pure Set.empty
      modify (\cache' -> Map.insert colour allHeldBags cache')
      pure $ allHeldBags

findAllHeldBags :: Map.Map String [(Int, String)] -> Map.Map String (Set.Set String)
findAllHeldBags rules = execState (forM colours (\colour -> findHeldBags rules colour)) Map.empty
  where
    colours = Map.keys rules

solve1 rules = length $ filter (\s -> Set.member "shiny gold" s) $ Map.elems heldBags
  where
    -- solve1 rules = heldBags

    -- solve1 rules = heldBags

    graph = makeGraph rules
    heldBags = findAllHeldBags graph

solution =
  Solution
    { _parse = parseFile "7.txt" parseInput,
      _solve1 = solve1,
      _solve2 = const ()
    }
