{-# LANGUAGE FlexibleContexts #-}

module Tokenizer where

import Text.Parsec
import Data.Functor.Identity
import Data.Either

data Token = Ident String
           | KeyWord String
           | Number Int
           deriving (Show, Eq)
           
-- http://zetcode.com/lang/python/lexicalstructure/
parseKeyWord :: Stream s m Char => ParsecT s u m Token
parseKeyWord = KeyWord <$> choice (try . string <$> keyWords)
    where
        keyWords = ["and", "del", "global", "not", "withas", "elif", "if", "or", "yieldassert", "else", "import", "pass", "Falsebreak", "except", "in", "raise", "Noneclass", "finally", "is", "return", "Truecontinue", "for", "lambda", "trydef", "from", "nonlocal", "while"]
        
-- https://docs.python.org/2.0/ref/integers.html
parseDecimalNumber :: Stream s m Char => ParsecT s u m Token
parseDecimalNumber = fmap (Number . read) $
    string "0" <|>
    do 
        let nonzerodigit = choice (char <$> "123456789")
        x <- nonzerodigit
        xs <- many digit
        return (x:xs)

-- https://www.tutorialspoint.com/python/python_basic_syntax.htm
parseIdentifier :: Stream s m Char => ParsecT s u m Token
parseIdentifier = Ident <$> do
    let allowsymbol = letter <|> char '_'
    x <- allowsymbol
    xs <- many (allowsymbol <|> digit)
    return (x:xs)           

(<||>) :: ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m ()
(<||>) a b = (const () <$> a) <|> (const () <$> b)
    
doAct :: Stream s m Char => ParsecT s u m Token
doAct = try (skip *> parseKeyWord       <* skip1) <|> 
            (skip *> parseDecimalNumber <* skip1) <|>
            (skip *> parseIdentifier    <* skip1)
            where
                skip = many $ char ' '
                skip1 = (many1 $ char ' ') <||> eof
                
    
tokenize :: String -> [Token]
tokenize input = fromRight [] $ parse (many doAct) "" input
