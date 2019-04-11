{-# Language InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Combinators
    ( Parser
    , runParser
    , streamCreate
    , char
    , string
    , some
    , many
    , digit
    , letter
    , choice
    , eof
    , parseList
    , space
    , satisfy
    , runParserUntilEof
    , expression
    )
where

-- import Prelude hiding ( fail )

import           Control.Applicative
import           Control.Monad                  ( void )
import qualified Control.Monad.Fail            as Fail
import           Data.Char
import           Data.Foldable
import           Data.Functor



-- -- Location is pair of line and column
data Location = Location Integer Integer deriving Show

locationLine :: Location -> Integer
locationLine (Location l _) = l

locationColumn :: Location -> Integer
locationColumn (Location _ c) = c

locationMoveLine :: Location -> Location
locationMoveLine (Location l _) = Location (l + 1) 0

locationMoveColumn :: Location -> Location
locationMoveColumn (Location l c) = Location l (c + 1)

-- -- Error must be of two types
data ParseError = ParseError String Location | AlternativeError [ParseError] Location deriving Show

errorUknown :: Location -> ParseError
errorUknown = ParseError ""

errorParse :: Location -> String -> ParseError
errorParse loc str = ParseError str loc

errorUnexpectedToken :: Show tok => Location -> tok -> ParseError
errorUnexpectedToken loc tok =
    errorParse loc ("unexpected token: " ++ show tok)

errorEof :: Location -> ParseError
errorEof loc = errorParse loc "eof reached"

-- Stream hold tokens and curent position
data Stream token = Stream {stream :: [token], position :: Location }

instance Show tok => Show (Stream tok) where
    show :: Stream tok -> String
    show (Stream xs loc) =
        "\nLine: "
            ++ show (locationLine loc)
            ++ "\nColumn: "
            ++ show (locationColumn loc)
            ++ "\nstream: "
            ++ show xs

streamCreate :: [tok] -> Stream tok
streamCreate xs = Stream xs (Location 0 0)

streamNextToken :: Stream tok -> Maybe tok
streamNextToken (Stream (x : xs) _) = Just x
streamNextToken (Stream []       _) = Nothing

streamConsume :: Stream Char -> Stream Char
streamConsume (Stream (x : xs) loc) = case x of
    '\n' -> Stream xs (locationMoveLine loc)
    _    -> Stream xs (locationMoveColumn loc)
streamConsume (Stream [] loc) = Stream [] loc

streamLocation :: Stream tok -> Location
streamLocation = position

-- Parser type
newtype Parser tok a = Parser { runParser :: Stream tok -> Either ParseError (Stream tok, a) }

instance Functor (Parser s) where
    fmap :: (a -> b) -> Parser s a -> Parser s b
    fmap f p = Parser $ \s -> case runParser p s of
        Left  err     -> Left err
        Right (s', a) -> Right (s', f a)


instance Applicative (Parser s) where
    (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
    p <*> q = Parser $ \s -> case runParser p s of
        Left  err        -> Left err
        Right (s', pfun) -> case runParser q s' of
            Left  err      -> Left err
            Right (s'', a) -> Right (s'', pfun a)

    pure :: a -> Parser s a
    pure a = Parser $ \s -> Right (s, a)


instance Monad (Parser s) where
    (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
    p >>= q = Parser $ \s -> case runParser p s of
        Left  err     -> Left err
        Right (s', a) -> case runParser (q a) s' of
            Left  err      -> Left err
            Right (s'', b) -> Right (s'', b)

    fail :: String -> Parser s a
    fail msg = Parser $ \s -> Left $ errorParse (streamLocation s) msg


instance Fail.MonadFail (Parser s) where
    fail :: String -> Parser s a
    fail msg = Parser $ \s -> Left $ errorParse (streamLocation s) msg


instance Alternative (Parser s) where
    (<|>) :: Parser s a -> Parser s a -> Parser s a
    p <|> q = Parser $ \s -> case runParser p s of
        Left err -> case runParser q s of
            Left err' -> Left $ merge err err' s
            x         -> x
        x -> x
      where
        merge e1@(ParseError _ _) e2@(ParseError _ _) s =
            AlternativeError [e1, e2] (streamLocation s)
        merge e1@(ParseError _ _) e2@(AlternativeError xs _) s =
            AlternativeError (e1 : xs) (streamLocation s)
        merge e1@(AlternativeError xs _) e2@(ParseError _ _) s =
            AlternativeError (xs ++ [e2]) (streamLocation s)
        merge e1@(AlternativeError xs1 _) e2@(AlternativeError xs2 _) s =
            AlternativeError (xs1 ++ xs2) (streamLocation s)

    empty :: Parser s a
    empty = Parser $ \s -> Left $ errorUknown (streamLocation s)


satisfy :: (Char -> Bool) -> Parser Char Char
satisfy pr = Parser $ \s -> case streamNextToken s of
    Just tok -> case pr tok of
        True  -> Right (streamConsume s, tok)
        False -> Left $ errorUnexpectedToken (streamLocation s) tok
    Nothing -> Left $ errorEof (streamLocation s)

token :: Char -> Parser Char Char
token ch = satisfy (== ch)

choice :: [Parser tok a] -> Parser tok a
choice = asum

digit :: Parser Char Char
digit = satisfy isDigit

char :: Char -> Parser Char Char
char c = satisfy (== c)

string :: String -> Parser Char String
string = foldr (\a b -> pure (:) <*> char a <*> b) (pure [])

letter :: Parser Char Char
letter = satisfy isLetter

space :: Parser Char ()
space = void $ satisfy isSpace

eof :: Parser Char ()
eof = Parser $ \s -> case streamNextToken s of
    Nothing -> Right (s, ())
    _       -> Left $ errorParse (streamLocation s) "expected eof"

try :: Parser tok a -> Parser tok (Maybe a)
try prs = fmap Just prs <|> pure Nothing

parseList
    :: Parser Char el
    -> Parser Char del
    -> Parser Char lbr
    -> Parser Char rbr
    -> (Int -> Bool)
    -> Parser Char [el]
parseList el del lbr rbr pr =
    let
        manySpaces = many space
        parseOther = (del >> manySpaces) *> el <* manySpaces
        parseFirst = manySpaces *> el <* manySpaces
    in
        (lbr >> manySpaces >> rbr >> (if pr 0 then return [] else fail "unexpected items in list")) <|>
        do
            lbr
            x  <- parseFirst
            xs <- many parseOther
            rbr
            let result = x : xs
            if pr $ length result
                then return result
                else fail "unexpected items in list"


data Assoc = LAssoc -- left associativity
           | RAssoc -- right associativity
           | NAssoc -- not associative

-- General parser combinator for expressions
-- Binary operators are listed in the order of precedence (from lower to higher)
-- Binary operators on the same level of precedence have the same associativity
-- Binary operator is specified with a parser for the operator itself and a semantic function to apply to the operands
expression :: forall ok a. [(Assoc, [(Parser Char ok, a -> a -> a)])] ->
              Parser Char a ->
              Parser Char a
expression ops primary = undefined
    where
        parseRecoursive :: [(Assoc, [(Parser Char ok, a -> a -> a)])] -> Parser Char a
        parseRecoursive ((LAssoc, parsers):xs) = do -- A \to A op B
            first <- many space *> parseRecoursive xs
            other <- many $ do
                op <- many space *> choice (fmap (uncurry ($>)) parsers)
                it <- many space *> parseRecoursive xs
                return (op, it)
            return $ foldl (\first (op, it) -> op first it) first other
        parseRecoursive ((RAssoc, parsers):xs) = undefined
        parseRecoursive ((NAssoc, parsers):xs) = undefined


runParserUntilEof :: Parser token ok -> [token] -> Either String ok
runParserUntilEof p inp =
  either (Left . show) (\(rest, ok) -> if null (stream rest) then Right ok else Left "Expected eof") (runParser p $ streamCreate inp)
