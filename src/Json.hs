module Json where

import Data.Char (isNumber)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))

import Parser

data Node = JsonNull | JsonBoolean Bool | JsonString String | JsonNumber Float | JsonList [Node] | JsonObject [(String, Node)]

nullParser :: Parser Char Node
nullParser = literalParser "null" >> return JsonNull

booleanParser :: Parser Char Node
booleanParser = (literalParser "true" >> return (JsonBoolean True)) <|> (literalParser "false" >> return (JsonBoolean False))

numberParser :: Parser Char Node
numberParser = fmap concat (sequence [digitsParser, fractionParser]) >>= \r -> return (JsonNumber (read r))

digitParser :: Parser Char Char
digitParser = predicateParser takeParser isNumber

digitsParser :: Parser Char String
digitsParser = atLeastOneParser digitParser

dotParser :: Parser Char String
dotParser = equalsParser '.' >> return ""

fractionParser :: Parser Char String
fractionParser = optionalParser (fmap concat (sequence [dotParser, digitsParser])) >>= \r -> return (fromMaybe "" r)

stringParser :: Parser Char String
stringParser = equalsParser '\"' >> charactersParser >>= (\r -> equalsParser '\"' >> return r)

charactersParser :: Parser Char String
charactersParser = manyParser characterParser

characterParser :: Parser Char Char
characterParser = rawCharacterParser <|> escapedCharacterParser

rawCharacterParser :: Parser Char Char
rawCharacterParser = predicateParser takeParser ((/=) '\"')

unescape :: Char -> Char
unescape '\\' = '\\'
unescape 'n' = '\n'
unescape c = c

escapedCharacterParser :: Parser Char Char
escapedCharacterParser = predicateParser takeParser ((==) '\\') >> fmap unescape takeParser
