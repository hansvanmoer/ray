module Json (JsonNode (JsonNull, JsonBoolean, JsonString, JsonNumber, JsonList, JsonObject), jsonParser, parseJson, testJson, JsonError) where

import Data.Maybe (fromMaybe)
import Data.List (find)
import Control.Applicative ((<|>))

import Parser
import Test

data JsonNode = JsonNull | JsonBoolean Bool | JsonString String | JsonNumber Float | JsonList [JsonNode] | JsonObject [(String, JsonNode)]

instance Eq JsonNode where
  (==) JsonNull JsonNull = True
  (==) JsonNull _ = False
  (==) (JsonBoolean l) (JsonBoolean r) = l == r
  (==) (JsonBoolean _) _ = False
  (==) (JsonNumber l) (JsonNumber r) = l == r
  (==) (JsonNumber _) _ = False
  (==) (JsonString l) (JsonString r) = l == r
  (==) (JsonString _) _ = False
  (==) (JsonList l) (JsonList r) = l == r
  (==) (JsonList _) _ = False
  (==) (JsonObject l) (JsonObject r) = l == r
  (==) (JsonObject _) _ = False

instance Show JsonNode where
  show JsonNull = "null"
  show (JsonBoolean b) = show b
  show (JsonNumber f) = show f
  show (JsonString s) = show s
  show (JsonList l) = show l
  show (JsonObject f) = "{" ++ show f ++ "}"

jsonNull :: JsonNode -> Maybe JsonNode
jsonNull JsonNull = Just JsonNull
jsonNull _ = Nothing

jsonBoolean :: JsonNode -> Maybe Bool
jsonBoolean (JsonBoolean b) = Just b
jsonBoolean _ = Nothing

jsonString :: JsonNode -> Maybe String
jsonString (JsonString s) = Just s
jsonString _ = Nothing

jsonNumber :: JsonNode -> Maybe Float
jsonNumber (JsonNumber f) = Just f
jsonNumber _ = Nothing

jsonList :: JsonNode -> Maybe [JsonNode]
jsonList (JsonList nodes) = Just nodes
jsonList _ = Nothing

jsonObject :: JsonNode -> Maybe [(String, JsonNode)]
jsonObject (JsonObject fields) = Just fields
jsonObject _ = Nothing

jsonField :: JsonNode -> String -> Maybe JsonNode
jsonField (JsonObject fields) key = fmap snd (find (\(k, _) -> k == key) fields)

jsonParser :: TextParser Char JsonNode
jsonParser = textParser rootParser isNewLine

type JsonError = Error

parseJson :: String -> Either JsonError JsonNode
parseJson = parseText jsonParser

isNewLine :: Char -> Bool
isNewLine '\n' = True
isNewLine _ = False

isWhitespace :: Char -> Bool
isWhitespace '\n' = True
isWhitespace ' ' = True
isWhitespace '\t' = True
isWhitespace _ = False

isDigit :: Char -> Bool
isDigit '0' = True
isDigit '1' = True
isDigit '2' = True
isDigit '3' = True
isDigit '4' = True
isDigit '5' = True
isDigit '6' = True
isDigit '7' = True
isDigit '8' = True
isDigit '9' = True
isDigit _ = False

rootParser :: Parser Char JsonNode
rootParser = whitespaceParser >> nodeParser >>= (\node -> whitespaceParser >> return node)

nodeParser :: Parser Char JsonNode
nodeParser = nullParser <|> booleanParser <|> stringParser <|> numberParser <|> listParser <|> objectParser

nullParser :: Parser Char JsonNode
nullParser = literalParser "null" >> return JsonNull

booleanParser :: Parser Char JsonNode
booleanParser = (literalParser "true" >> return (JsonBoolean True)) <|>
                (literalParser "false" >> return (JsonBoolean False))

numberParser :: Parser Char JsonNode
numberParser = digitsParser <++> fractionParser >>= return . JsonNumber . read

digitParser :: Parser Char Char
digitParser = predicateParser takeParser isDigit

digitsParser :: Parser Char String
digitsParser = atLeastOneParser digitParser

dotParser :: Parser Char Char
dotParser = equalsParser '.'

fractionParser :: Parser Char String
fractionParser = optionalParser (dotParser <:> digitsParser) >>= return . (fromMaybe "")

stringLiteralParser :: Parser Char String
stringLiteralParser = equalsParser '\"' >> charactersParser >>= (\r -> equalsParser '\"' >> return r)

stringParser :: Parser Char JsonNode
stringParser = fmap JsonString stringLiteralParser

charactersParser :: Parser Char String
charactersParser = manyParser characterParser

characterParser :: Parser Char Char
characterParser = escapedCharacterParser <|> rawCharacterParser

rawCharacterParser :: Parser Char Char
rawCharacterParser = predicateParser takeParser ((/=) '\"')

unescape :: Char -> Char
unescape '\\' = '\\'
unescape 'n' = '\n'
unescape c = c

escapedCharacterParser :: Parser Char Char
escapedCharacterParser = predicateParser takeParser ((==) '\\') >> fmap unescape takeParser

whitespaceParser :: Parser Char String
whitespaceParser = manyParser (predicateParser takeParser isWhitespace)

listParser :: Parser Char JsonNode
listParser = listHeadParser >>
             whitespaceParser >>
             (
               (nodeParser <:> listTailParser) <|>
               listEndParser
             ) >>=
             return . JsonList

listHeadParser :: Parser Char Char
listHeadParser = equalsParser '['

listEndParser :: Parser Char [JsonNode]
listEndParser = equalsParser ']' >> return []

listTailParser :: Parser Char [JsonNode]
listTailParser = whitespaceParser >>
                 (
                   listEndParser <|>
                   (separatorParser >> whitespaceParser >> (nodeParser <:> listTailParser))
                 )

separatorParser :: Parser Char Char
separatorParser = equalsParser ','

objectParser :: Parser Char JsonNode
objectParser = objectStartParser >>
               whitespaceParser >>
               (
                 (fieldParser <:> objectTailParser) <|>
                 objectEndParser
               ) >>=
               return . JsonObject

objectTailParser :: Parser Char [(String, JsonNode)]
objectTailParser = whitespaceParser >>
                   (
                     objectEndParser <|>
                     (separatorParser >> whitespaceParser >> (fieldParser <:> objectTailParser))
                   )

fieldParser :: Parser Char (String, JsonNode)
fieldParser = whitespaceParser >>
              stringLiteralParser >>=
              (\key -> whitespaceParser >>
                       equalsParser ':' >>
                       whitespaceParser >>
                       nodeParser >>=
                       (\value -> return (key, value))
              )

objectStartParser :: Parser Char Char
objectStartParser = equalsParser '{'

objectEndParser :: Parser Char [(String, JsonNode)]
objectEndParser = equalsParser '}' >> return []

testJson :: Test
testJson = testSuite "Json" [
  testJsonNull, testJsonNotNull,
  testJsonBoolean, testJsonBoolean,
  testJsonNumber, testJsonNotNumber,
  testJsonString, testJsonNotString,
  testJsonList, testJsonNotList,
  testJsonObject, testJsonNotObject,
  testJsonField, testJsonFieldNotFound,
  testParseNull,
  testParseTrue, testParseFalse,
  testParseInteger, testParseFraction,
  testParseEmptyList, testParseEmptyListWithWhitespace, testParseNonEmptyList,
  testParseEmptyString, testParseString, testParseEscapedString,
  testParseEmptyObject, testParseEmptyObjectWithWhitespace, testParseObject,
  testParseLeadingAndTrailingWhitespace
  ]

testParseNull :: Test
testParseNull = test "Parse Null" (assertEquals expected (parseText jsonParser input))
  where
    expected = Right JsonNull
    input = "null"

testParseTrue :: Test
testParseTrue = test "Parse True" (assertEquals expected (parseText jsonParser input))
  where
    expected = Right (JsonBoolean True)
    input = "true"

testParseFalse :: Test
testParseFalse = test "Parse True" (assertEquals expected (parseText jsonParser input))
  where
    expected = Right (JsonBoolean False)
    input = "false"

testParseInteger :: Test
testParseInteger = test "Parse Integer" (assertEquals expected (parseText jsonParser input))
  where
    expected = Right (JsonNumber 12.0)
    input = "12"

testParseFraction :: Test
testParseFraction = test "Parse Fraction" (assertEquals expected (parseText jsonParser input))
  where
    expected = Right (JsonNumber 12.456)
    input = "12.456"


testParseEmptyString :: Test
testParseEmptyString = test "Parse empty string" (assertEquals expected (parseText jsonParser input))
  where
    expected = Right (JsonString "")
    input = "\"\""

testParseString :: Test
testParseString = test "Parse string" (assertEquals expected (parseText jsonParser input))
  where
    expected = Right (JsonString "abc")
    input = "\"abc\""

testParseEscapedString :: Test
testParseEscapedString = test "Parse escaped string" (assertEquals expected (parseText jsonParser input))
  where
    expected = Right (JsonString ("ab" ++ "\n" ++ "\\" ++ "\"" ++ "c"))
    input = "\"ab" ++ "\\n" ++ "\\\\" ++ "\\\"" ++ "c\""

testParseEmptyList :: Test
testParseEmptyList = test "Parse Empty List" (assertEquals expected (parseText jsonParser input))
  where
    expected = Right (JsonList [])
    input = "[]"

testParseEmptyListWithWhitespace :: Test
testParseEmptyListWithWhitespace = test "Parse empty list with whitespace" (assertEquals expected (parseText jsonParser input))
  where
    expected = Right (JsonList [])
    input = "[  ]"


testParseNonEmptyList :: Test
testParseNonEmptyList = test "Parse non-empty List" (assertEquals expected (parseText jsonParser input))
  where
    expected = Right (JsonList [JsonNull, (JsonBoolean False), (JsonNumber 1.0), (JsonNumber 2.0), (JsonString "test")])
    input = "[null , false,1.0 ,2.0 ,\"test\"]"

testParseEmptyObject :: Test
testParseEmptyObject = test "Parse empty object" (assertEquals expected (parseText jsonParser input))
  where
    expected = Right (JsonObject [])
    input = "{}"

testParseEmptyObjectWithWhitespace :: Test
testParseEmptyObjectWithWhitespace = test "Parse empty object with whitespace" (assertEquals expected (parseText jsonParser input))
  where
    expected = Right (JsonObject [])
    input = "{  }"

testParseObject :: Test
testParseObject = test "Parse object" (assertEquals expected (parseText jsonParser input))
  where
    expected = Right (
      JsonObject [
          ("boolean", JsonBoolean True),
          ("number", JsonNumber 1.0),
          ("null", JsonNull),
          ("string", JsonString "te\nst"),
          ("list", JsonList [JsonNull, (JsonBoolean False), (JsonNumber 1.0), (JsonNumber 2.0), (JsonString "test")]),
          ("object", JsonObject [("field", JsonBoolean True)])
          ]
      )
    input = "{" ++
      "\"boolean\":true," ++
      "\"number\" :1.0," ++
      "\"null\" : null," ++
      "\"string\" : \"te\\nst\" ," ++
      "\"list\" : [null,false,1.0,2.0,\"test\"] , " ++
      "\"object\" : {\"field\":true}" ++
      "}"

testParseLeadingAndTrailingWhitespace :: Test
testParseLeadingAndTrailingWhitespace =  test "Parse object" (assertEquals expected (parseText jsonParser input))
  where
    expected = Right (JsonString "test")
    input = " \n   \"test\"  \n "

testJsonNull :: Test
testJsonNull = test "jsonNull" (assertEquals (Just JsonNull) (jsonNull JsonNull))

testJsonNotNull :: Test
testJsonNotNull = test "jsonNull not null" (assertEquals Nothing (jsonNull (JsonString "test")))

testJsonBoolean :: Test
testJsonBoolean = test "jsonBoolean" (assertEquals (Just True) (jsonBoolean (JsonBoolean True)))

testJsonNotBoolean :: Test
testJsonNotBoolean = test "jsonBoolean not a boolean" (assertEquals Nothing (jsonBoolean JsonNull))

testJsonNumber :: Test
testJsonNumber = test "jsonNumber" (assertEquals (Just 1.0) (jsonNumber (JsonNumber 1.0)))

testJsonNotNumber :: Test
testJsonNotNumber = test "jsonNumber not a number" (assertEquals Nothing (jsonNumber JsonNull))

testJsonString :: Test
testJsonString = test "jsonString" (assertEquals (Just "test") (jsonString (JsonString "test")))

testJsonNotString :: Test
testJsonNotString = test "jsonString not a string" (assertEquals Nothing (jsonString JsonNull))

testJsonList :: Test
testJsonList = test "jsonList" (assertEquals (Just [JsonNull]) (jsonList (JsonList [JsonNull])))

testJsonNotList :: Test
testJsonNotList = test "jsonList not a list" (assertEquals Nothing (jsonList JsonNull))

testJsonObject :: Test
testJsonObject = test "jsonObject" (assertEquals (Just [("field", JsonNull)]) (jsonObject (JsonObject [("field", JsonNull)])))

testJsonNotObject :: Test
testJsonNotObject = test "jsonObject not an object" (assertEquals Nothing (jsonObject JsonNull))

testJsonField :: Test
testJsonField = test "jsonField" (assertEquals (Just (JsonBoolean True)) (jsonField object "field2"))
  where
    object = JsonObject [("field1", JsonNumber 1.0), ("field2", JsonBoolean True), ("field3", JsonBoolean False)]

testJsonFieldNotFound :: Test
testJsonFieldNotFound = test "jsonField" (assertEquals Nothing (jsonField object "field4"))
  where
    object = JsonObject [("field1", JsonNumber 1.0), ("field2", JsonBoolean True), ("field3", JsonBoolean False)]
