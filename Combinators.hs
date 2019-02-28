module Combinators (Parser, keywords, char, string, some, many, runParser, digit, letter, choice, eof, parseList, fail, space) where

import Data.Char
import qualified Prelude
import Prelude hiding (fail, fmap, (<*>), (>>=))
import Control.Applicative (Alternative)
import qualified Control.Applicative (empty, (<|>))


-- Parsing result is some payload and a suffix of the input which is yet to be parsed
newtype Parser str ok = Parser { runParser :: str -> Maybe (str, ok) }

-- Parser which always succeedes consuming no input
success :: ok -> Parser str ok
success ok = Parser $ \s -> Just (s, ok)

-- Parser which fails no mater the input
fail :: Parser str ok
fail = Parser $ const Nothing

-- Biased choice: if the first parser succeedes, the second is never run
(<|>) :: Parser str ok -> Parser str ok -> Parser str ok
p <|> q = Parser $ \s ->
  case runParser p s of
    Nothing -> runParser q s
    x -> x

-- Default sequence combinator
-- If the first parser succeedes then the second parser is used
-- If the first does not succeed then the second one is never tried
-- The result is collected into a pair
seq :: Parser str a -> Parser str b -> Parser str (a, b)
p `seq` q = Parser $ \s ->
  case runParser p s of
       Nothing -> Nothing
       Just (pstr, pres) -> case runParser q pstr of
                                 Nothing -> Nothing
                                 Just (qstr, qres) -> Just (qstr, (pres, qres))

-- Monadic sequence combinator
(>>=) :: Parser str a -> (a -> Parser str b) -> Parser str b
p >>= q = Parser $ \s ->
  case runParser p s of
       Nothing -> Nothing
       Just (pstr, pres) -> case runParser (q pres) pstr of
                                 Nothing -> Nothing
                                 Just (qstr, qres) -> Just (qstr, qres)

-- Applicative sequence combinator
(<*>) :: Parser str (a -> b) -> Parser str a -> Parser str b
p <*> q = Parser $ \s ->
  case runParser p s of
       Nothing -> Nothing
       Just (pstr, pfun) -> case runParser q pstr of
                                 Nothing -> Nothing
                                 Just (qstr, qres) -> Just (qstr, pfun qres)

-- Applies a function to the parsing result, if parser succeedes
fmap :: (a -> b) -> Parser str a -> Parser str b
fmap f p = Parser $ \s ->
  case runParser p s of
    Just (s', a) -> Just (s', f a)
    _ -> Nothing

-- Applies a parser zero or more times
many :: Parser str a -> Parser str [a]
many p = many_v where
  many_v = some_v <|> success []
  some_v = (fmap (:) p) <*> many_v
  
-- Applies a parser once or more times
some :: Parser str a -> Parser str [a]
some p = (fmap (:) p) <*> many p

choice :: [Parser str a] -> Parser str a
choice = foldr (<|>) fail

satisfy :: (token -> Bool) -> Parser [token] token
satisfy pr = Parser f 
  where
    f (c:cs) | pr c  = Just (cs,c)
    f _              = Nothing

-- Checks if the first element of the input is the given token
token :: Eq token => token -> Parser [token] token
token t = satisfy (==t)

digit :: Parser String Char
digit = satisfy isDigit 

letter :: Parser String Char
letter = satisfy isLetter

space :: Parser String ()
space = const () <$> satisfy isSpace

-- Checks if the first character of the string is the one given
char :: Char -> Parser String Char
char = token

string :: String -> Parser String String
string s = foldr (\a b -> pure (:) <*> char a <*> b) (pure []) s

eof :: Parser [token] ()
eof = Parser $ \s -> case s of
                          [] -> Just ([], ())
                          _  -> Nothing

-- Parses keywords 
keywords :: [String] -> Parser String String
keywords kws = Parser $ \s -> eattokens s fullTrie ""
    where
      fullTrie = foldl add (Trie False []) kws
      
      eattokens [] (Trie True _) str = Just ([], str)
      eattokens [] _ _ = Nothing
      eattokens l@(' ':xs) (Trie True _) str = Just (l, str)
      eattokens l@(' ':xs) _ _ = Nothing
      eattokens l@(x:xs) trie str = case get trie x of
                                         Nothing -> Nothing
                                         Just ve -> eattokens xs ve (str ++ [x])

data Trie key = Trie Bool [(key, Trie key)]

add :: (Eq k) => Trie k -> [k] -> Trie k
add (Trie _ edges) []     = Trie True edges
add (Trie t edges) (x:xs) = case lookup x edges of
                                 Nothing -> Trie t $ (x, add (Trie False []) xs):edges
                                 Just ve -> Trie t $ modify <$> edges
                                  where
                                    modify (xkey, trie) = if xkey == x 
                                                             then (xkey, add trie xs)
                                                             else (xkey, trie)

get :: (Eq k) => Trie k -> k -> Maybe (Trie k)
get (Trie _ edges) key = lookup key edges

try :: Parser String a -> Parser String (Maybe a)
try prs = fmap Just prs <|> success Nothing

parseList :: Parser String el -> Parser String del -> Parser String lbr -> Parser String rbr -> (Int -> Bool) -> Parser String [el]
parseList el del lbr rbr pr = do
    let manySpaces = many $ space
    let parseItem = manySpaces *> el <* (manySpaces >> del)
    let parseLastItem = manySpaces *> el <* manySpaces
    
    lbr
    xs <- many parseItem
    x  <- try parseLastItem
    rbr
    
    let result = case x of
                    Nothing   -> xs
                    Just item -> xs ++ [item]
    
    if pr $ length result
    then
        return result
    else
        fail


instance Functor (Parser s) where
  fmap = fmap
  
instance Applicative (Parser s) where
  pure = success
  (<*>) = (<*>)
  
instance Alternative (Parser s) where
  empty = fail
  (<|>) = (<|>)
  
instance Monad (Parser s) where
  return = pure
  (>>=) = (>>=)
  fail = const fail
