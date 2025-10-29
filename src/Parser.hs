module Parser (Result, Parser, atLeastOneParser, branchParser, equalsParser, literalParser, makeParser, manyParser, optionalParser, predicateParser, runParser, takeParser, TextParser, parseText, testParser, textParser, Error, (<++>), (<:>)) where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Monad (MonadPlus, mzero, mplus, sequence)

import Test

-- The result returned by a parser
type Result t a = Either (String, [t]) (a, [t])

mapResult :: (a -> b) -> Result t a -> Result t b
mapResult f (Right (value, remainder)) = Right (f value, remainder)
mapResult _ (Left (error, remainder)) = Left (error, remainder)

-- A parser
newtype Parser t a = P ([t] -> Result t a)

-- Constructs a new parser
makeParser :: ([t] -> Result t a) -> Parser t a
makeParser = P

-- Runs a parser
runParser :: Parser t a -> [t] -> Result t a
runParser (P p) input = p input

instance Functor (Parser t) where
  -- fmap :: (a -> b) -> Parser t a -> Parser t b
  fmap f p = makeParser (\input -> mapResult f (runParser p input))

instance Applicative (Parser t) where
  -- pure :: a -> Parser t a
  pure v = makeParser (\input -> Right (v, input))

  -- (<*>) :: Parser t (a -> b) -> Parser t a
  (<*>) l r = makeParser parse
    where
      parse input = do
        (f, remainder) <- runParser l input
        runParser (fmap f r) remainder

instance Monad (Parser t) where
  -- return :: a -> Parser t a
  return = pure

  -- (>>=) :: (Parser t a) -> (a -> Parser t b) -> Parser t b
  (>>=) p f = makeParser parse
    where
      parse input = do
        (value, remainder) <- runParser p input
        runParser (f value) remainder

instance Alternative (Parser t) where
  -- empty :: Parser t a
  empty = makeParser (\input -> Left ("Nothing parsed", input))

  -- (<|>) :: Parser t a -> Parser t a -> Parser t a
  (<|>) l r = makeParser (\input -> combine (runParser l input) (runParser r input))
    where
      combine (Left ll) (Left _) = Left ll
      combine (Left _) rr = rr
      combine ll _ = ll

instance MonadPlus (Parser t) where
  mzero = empty
  mplus = (<|>)

instance MonadFail (Parser t) where
  -- fail :: String -> Parser t a
  fail msg = makeParser (\input -> Left (msg, input))

(<++>) :: Parser t [a] -> Parser t [a] -> Parser t [a]
(<++>) l r = l >>= (\result -> fmap ((++) result) r)

(<:>) :: Parser t a -> Parser t [a] -> Parser t [a]
(<:>) l r = pure (:) <*> l <*> r

-- Parses nothing and returns a constant
constantParser :: a -> Parser t a
constantParser c = makeParser (\input -> Right (c, input))

-- Takes a single token from the stream
takeParser :: Parser t t
takeParser = makeParser parse
  where
    parse [] = Left ("Unexpected end of stream, expected at least one token", [])
    parse (x:xs) = pure (x, xs)

testTake :: Test
testTake = testSuite "Take" [testTakeNonEmpty, testTakeEmpty]

testTakeNonEmpty :: Test
testTakeNonEmpty = test "Non empty" (assertEquals expected (runParser parser input))
  where
    expected = pure ('a', "bc")
    parser = takeParser
    input = "abc"

testTakeEmpty :: Test
testTakeEmpty = test "Empty" (assertEquals expected (runParser parser input))
  where
    expected = Left ("Unexpected end of stream, expected at least one token", "")
    parser = takeParser
    input = ""

-- Takes a single token from the stream and fails if it does not fit the predicate
predicateParser :: (Show t) => Parser t t -> (t -> Bool) -> Parser t t
predicateParser p pred = makeParser parse
  where
    parse [] = Left ("Unexpected end of stream, expected at least one token", [])
    parse (x:xs) = if pred x
                   then runParser p (x:xs)
                   else Left ("Unexpected token " ++ show x, (x:xs))

testPredicate :: Test
testPredicate = testSuite "Predicate" [testPredicateSuccess, testPredicateFailure, testPredicateEmpty]

testPredicateSuccess :: Test
testPredicateSuccess = test "Matches" (assertEquals expected (runParser parser input))
  where
    expected = pure ('a', "bc")
    parser = predicateParser takeParser ((==) 'a')
    input = "abc"

testPredicateFailure :: Test
testPredicateFailure = test "Does not match" (assertEquals expected (runParser parser input))
  where
    expected = Left ("Unexpected token 'a'", "abc")
    parser = predicateParser takeParser ((==) 'b')
    input = "abc"

testPredicateEmpty :: Test
testPredicateEmpty = test "Empty" (assertEquals expected (runParser parser input))
  where
    expected = Left ("Unexpected end of stream, expected at least one token", "")
    parser = predicateParser takeParser ((==) 'a')
    input = ""

-- Takes a single token from the stream and fails if it is not equal to the supplied token
equalsParser :: (Eq t, Show t) => t -> Parser t t
equalsParser t = takeParser >>= pred
  where
    pred c = if c == t
             then constantParser c
             else fail ("Unexpected token " ++ show c ++ ", expected " ++ show t)


testEquals :: Test
testEquals = testSuite "Equals" [testEqualsSuccess, testEqualsFailure, testEqualsEmpty]

testEqualsSuccess :: Test
testEqualsSuccess = test "Equal" (assertEquals expected (runParser parser input))
  where
    expected = pure ('a', "bc")
    parser = equalsParser 'a'
    input = "abc"

testEqualsFailure :: Test
testEqualsFailure = test "Not equal" (assertEquals expected (runParser parser input))
  where
    expected = Left ("Unexpected token 'a', expected 'b'", "bc")
    parser = equalsParser 'b'
    input = "abc"

testEqualsEmpty :: Test
testEqualsEmpty = test "Empty" (assertEquals expected (runParser parser input))
  where
    expected = Left ("Unexpected end of stream, expected at least one token", "")
    parser = equalsParser 'a'
    input = ""
    
-- Repeats the parser zero or one times
optionalParser :: Parser t a -> Parser t (Maybe a)
optionalParser p = fmap Just p <|> pure Nothing

testOptional :: Test
testOptional = testSuite "Optional" [testOptionalSome, testOptionalNone]

testOptionalSome :: Test
testOptionalSome = test "Some" (assertEquals expected (runParser parser input))
  where
    expected = pure (Just 'a', "bc")
    parser = optionalParser (equalsParser 'a')
    input = "abc"

testOptionalNone:: Test
testOptionalNone = test "None" (assertEquals expected (runParser parser input))
  where
    expected = Right (Nothing, "abc")
    parser = optionalParser (equalsParser 'b')
    input = "abc"

-- Repeats the parser zero or more times
manyParser :: Parser t a -> Parser t [a]
manyParser p = atLeastOneParser p <|> pure []

testMany :: Test
testMany = testSuite "Many" [testManyNone, testManyOne, testManyMore]

testManyNone :: Test
testManyNone = test "None" (assertEquals expected (runParser parser input))
  where
    expected = pure ("", "abc")
    parser = manyParser (equalsParser 'b')
    input = "abc"

testManyOne :: Test
testManyOne = test "One" (assertEquals expected (runParser parser input))
  where
    expected = pure ("a", "bc")
    parser = manyParser (equalsParser 'a')
    input = "abc"

testManyMore :: Test
testManyMore = test "More" (assertEquals expected (runParser parser input))
  where
    expected = pure ("aa", "bc")
    parser = manyParser (equalsParser 'a')
    input = "aabc"

-- Repeats the parser one or more time
atLeastOneParser :: Parser t a -> Parser t [a]
atLeastOneParser p = pure (:) <*> p <*> manyParser p

-- Chooses the first successful branch
branchParser :: [Parser t a] -> Parser t a
branchParser parsers = foldr (<|>) empty parsers

testAtLeastOne :: Test
testAtLeastOne = testSuite "Many" [testAtLeastOneNone, testAtLeastOneEmpty, testAtLeastOneOne, testAtLeastOneMore]

testAtLeastOneNone :: Test
testAtLeastOneNone = test "None" (assertEquals expected (runParser parser input))
  where
    expected = Left ("Unexpected token 'a', expected 'b'", "bc")
    parser = atLeastOneParser (equalsParser 'b')
    input = "abc"

testAtLeastOneEmpty :: Test
testAtLeastOneEmpty = test "Empty" (assertEquals expected (runParser parser input))
  where
    expected = Left ("Unexpected end of stream, expected at least one token", "")
    parser = atLeastOneParser (equalsParser 'a')
    input = ""
    
testAtLeastOneOne :: Test
testAtLeastOneOne = test "One" (assertEquals expected (runParser parser input))
  where
    expected = pure ("a", "bc")
    parser = atLeastOneParser (equalsParser 'a')
    input = "abc"

testAtLeastOneMore :: Test
testAtLeastOneMore = test "More" (assertEquals expected (runParser parser input))
  where
    expected = pure ("aa", "bc")
    parser = atLeastOneParser (equalsParser 'a')
    input = "aabc"

literalParser :: (Eq t, Show t) => [t] -> Parser t [t]
literalParser lit = sequence (fmap equalsParser lit)

testLiteral :: Test
testLiteral = testSuite "Literal" [testLiteralMatches, testLiteralDoesNotMatch]

testLiteralMatches :: Test
testLiteralMatches = test "Matches" (assertEquals expected (runParser parser input))
  where
    expected = pure ("abc", "de")
    parser = literalParser "abc"
    input = "abcde"

testLiteralDoesNotMatch :: Test
testLiteralDoesNotMatch = test "Does not match" (assertEquals expected (runParser parser input))
  where
    expected = Left ("Unexpected token 'd', expected 'c'", "cef")
    parser = literalParser "abc"
    input = "abdcef"


-- A basic error type for a text parser
data Error = E String Int Int

instance Eq Error where
  (==) (E lm ll lc) (E rm rl rc) = lm == rm && ll == rl && lc == rc

instance Show Error where
  show (E m l c) = m ++ ", at (" ++ show l ++ ", " ++ show c ++ ")"

-- Fetches the error message
errorMessage :: Error -> String
errorMessage (E msg _ _) = msg

-- Fetches the error line
errorLine :: Error -> Int
errorLine (E _ l _) = l

-- Fetches the error column
errorColumn :: Error -> Int
errorColumn (E _ _ c) = c

-- A text parser has notions of lines and columns and requires the entire string to be consumed
data TextParser t a = T (Parser t a) (t -> Bool)

-- Creates a text parser
textParser :: Parser t a -> (t -> Bool) -> TextParser t a
textParser = T

-- Fully parses a string
parseText :: TextParser t a -> [t] ->  Either Error a
parseText (T p linePred) input = handleError (runParser p input)
  where
    handleError (Left (msg, remainder)) = makeError msg (calculateCoords remainder)
    handleError (Right (result, [])) = Right result
    handleError (Right (_, remainder)) = makeError "There are unparsed tokens left" (calculateCoords remainder)
    makeError msg (l, c) = Left (E msg l c)
    calculateCoords remainder = calculateCoords' (1, 1) input (length input - length remainder)
    calculateCoords' r [] _ = r
    calculateCoords' r _ 0 = r
    calculateCoords' (l, c) (x:xs) i = calculateCoords' (if linePred x then (l + 1, 0) else (l, c + 1)) xs (i - 1)

testParseText :: Test
testParseText = testSuite "Text" [testParseTextSuccess, testParseTextFailure, testParseTextHasMore]

testParseTextSuccess :: Test
testParseTextSuccess = test "Success" (assertEquals result (parseText parser input))
  where
    result = Right "abc"
    parser = textParser (literalParser "abc") ((==) '\n')
    input = "abc"

testParseTextFailure :: Test
testParseTextFailure = test "Failure" (assertEquals result (parseText parser input))
  where
    result = Left (E "Unexpected token 'b', expected 'd'" 2 2)
    parser = textParser (literalParser "d\nadc") ((==) '\n')
    input = "d\nabc"

testParseTextHasMore :: Test
testParseTextHasMore = test "Has more" (assertEquals result (parseText parser input))
  where
    result = Left (E "There are unparsed tokens left" 1 4)
    parser = textParser (literalParser "abc") ((==) '\n')
    input = "abcdef"


testParser :: Test
testParser = testSuite "Parser" [testTake, testPredicate, testEquals, testLiteral, testOptional, testMany, testAtLeastOne, testParseText]
