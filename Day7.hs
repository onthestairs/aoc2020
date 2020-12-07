{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day7 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt)
import Control.Lens hiding (contains)
import qualified Data.Map.Strict as Map
import qualified Data.MultiSet as Set
import Relude
import Text.Megaparsec (eof, sepBy1)
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
  pure (n, colour)

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

findHeldBags :: Map.Map String [(Int, String)] -> String -> State (Map.Map String (Set.MultiSet String)) (Set.MultiSet String)
findHeldBags rules colour = do
  cache <- get
  case Map.lookup colour cache of
    Just c -> pure c
    Nothing -> do
      allHeldBags <- case Map.lookup colour rules of
        Just cs -> foldlM combineBags Set.empty cs
        Nothing -> pure Set.empty
      modify (Map.insert colour allHeldBags)
      pure allHeldBags
  where
    combineBags heldBags (n, heldColour) = do
      subBags <- findHeldBags rules heldColour
      let allSubBags = Set.unions $ replicate n subBags
      pure $ Set.union heldBags $ Set.insertMany heldColour n allSubBags

findAllHeldBags :: Map.Map String [(Int, String)] -> Map.Map String (Set.MultiSet String)
findAllHeldBags rules = execState (forM colours (\colour -> findHeldBags rules colour)) Map.empty
  where
    colours = Map.keys rules

solve1 rules = length $ filter (Set.member "shiny gold") $ Map.elems heldBags
  where
    graph = makeGraph rules
    heldBags = findAllHeldBags graph

solve2 rules = Set.size <$> Map.lookup "shiny gold" heldBags
  where
    graph = makeGraph rules
    heldBags = findAllHeldBags graph

solution =
  Solution
    { _parse = parseFile "7.txt" parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
