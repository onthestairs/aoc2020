{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day4 (solution) where

import AOC (Parser, Solution (..), parseFile)
import Control.Lens
import qualified Data.Map.Strict as Map
import Relude
import Text.Megaparsec (anySingle, choice, eof, lookAhead, sepBy1, try)
import Text.Megaparsec.Char (alphaNumChar, char, newline, string)

data RawDocument = RawDocument
  { _birthYear :: Maybe String, -- byr (Birth Year)
    _issueYear :: Maybe String, -- iyr (Issue Year)
    _expirationYear :: Maybe String, -- eyr (Expiration Year)
    _height :: Maybe String, -- hgt (Height)
    _hairColour :: Maybe String, -- hcl (Hair Color)
    _eyeColour :: Maybe String, -- ecl (Eye Color)
    _passportID :: Maybe String, -- pid (Passport ID)
    _countryID :: Maybe String --cid (Country ID)
  }
  deriving (Show)

makeLenses ''RawDocument

type Input = [RawDocument]

toRawDocument m =
  RawDocument
    { _birthYear = Map.lookup "byr" m,
      _issueYear = Map.lookup "iyr" m,
      _expirationYear = Map.lookup "eyr" m,
      _height = Map.lookup "hgt" m,
      _hairColour = Map.lookup "hcl" m,
      _eyeColour = Map.lookup "ecl" m,
      _passportID = Map.lookup "pid" m,
      _countryID = Map.lookup "cid" m
    }

twoNewlines = do
  newline
  newline

parseInput :: Parser Input
parseInput = map toRawDocument <$> parseMaps

parseMaps = sepBy1 parseDocumentMap twoNewlines <* eof

parseKV :: Parser (String, String)
parseKV = do
  key <- choice $ map string ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]
  char ':'
  value <- some (alphaNumChar <|> char '#')
  pure (toString key, value)

spaceDelimeter = char ' ' <|> newline

sepBySpace :: Parser a -> Parser [a]
sepBySpace p = do
  x <- p
  xs <- many $
    try $ do
      spaceDelimeter
      c <- lookAhead anySingle
      when (c == '\n') (fail "No more KVS")
      p
  pure $ x : xs

parseDocumentMap :: Parser (Map.Map String String)
parseDocumentMap = do
  kvs <- sepBySpace parseKV
  pure $ Map.fromList kvs

hasNecessaryFields
  RawDocument
    { _birthYear = Just _,
      _issueYear = Just _,
      _expirationYear = Just _,
      _height = Just _,
      _hairColour = Just _,
      _eyeColour = Just _,
      _passportID = Just _,
      _countryID = _
    } = True
hasNecessaryFields _ = False

solve1 documents = length $ filter hasNecessaryFields documents

data Document = Document
  { _birthYear' :: Int, -- byr (Birth Year)
    _issueYear' :: Int, -- iyr (Issue Year)
    _expirationYear' :: Int, -- eyr (Expiration Year)
    _height' :: String, -- hgt (Height)
    _hairColour' :: String, -- hcl (Hair Color)
    _eyeColour' :: String, -- ecl (Eye Color)
    _passportID' :: Int, -- pid (Passport ID)
    _countryID' :: Maybe String --cid (Country ID)
  }
  deriving (Show)

checkPredicate :: (a -> Bool) -> a -> Maybe a
checkPredicate p x = if p x then Just x else Nothing

isValidHeight [a, b, c, 'c', 'm'] = fromMaybe False $ fmap (\x -> x >= 150 && x <= 193) (readMaybe (a : b : c : []) :: Maybe Int)
isValidHeight (a : b : "in") = fromMaybe False $ fmap (\x -> x >= 59 && x <= 76) (readMaybe (a : b : []) :: Maybe Int)
isValidHeight _ = False

isValidHairColour ('#' : hex) = length hex == 6 && all (\c -> c `elem` ("0123456789abcdef" :: String)) hex
isValidHairColour _ = False

isValidPassportID :: String -> Bool
isValidPassportID passportID = length passportID == 9 && all (\c -> c `elem` ("0123456789" :: String)) passportID

validateDocument :: RawDocument -> Maybe Document
validateDocument rawDoc = do
  birthYear' <- view birthYear rawDoc >>= readMaybe >>= checkPredicate (\year -> year >= 1920 && year <= 2002)
  issueYear' <- view issueYear rawDoc >>= readMaybe >>= checkPredicate (\year -> year >= 2010 && year <= 2020)
  expirationYear' <- view expirationYear rawDoc >>= readMaybe >>= checkPredicate (\year -> year >= 2020 && year <= 2030)
  height' <- view height rawDoc >>= checkPredicate isValidHeight
  hairColour' <- view hairColour rawDoc >>= checkPredicate isValidHairColour
  eyeColour' <- view eyeColour rawDoc >>= checkPredicate (\eyeColour -> eyeColour `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
  passportID' <- view passportID rawDoc >>= checkPredicate isValidPassportID >>= readMaybe
  pure $
    Document
      { _birthYear' = birthYear',
        _issueYear' = issueYear',
        _expirationYear' = expirationYear',
        _height' = height',
        _hairColour' = hairColour',
        _eyeColour' = eyeColour',
        _passportID' = passportID',
        _countryID' = view countryID rawDoc
      }

solve2 rawDocs = length $ mapMaybe validateDocument rawDocs

solution =
  Solution
    { _parse = parseFile "4.txt" parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
