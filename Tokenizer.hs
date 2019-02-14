module Tokenizer where

import Combinators (Parser, keywords, char, string, some, many, runParser, digit, letter, choice, eof)
import Control.Applicative ((<|>))

data Token = Ident String
           | KeyWord String
           | Number Int  -- Change Number type if you work with something other than Int
           deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize input = case runParser (many doAct <* eof) input of
                      Just (_, tokens) -> tokens
                      _ -> []

parseIdent :: Parser String String
parseIdent = do
    let allowsymbol = letter <|> char '_'
    x <- allowsymbol
    xs <- many (allowsymbol <|> digit)
    return (x:xs)

parseKeyWord :: Parser String String
parseKeyWord = keywords ["and", "del", "global", "not", "withas", "elif", "if", "or", "yieldassert", "else", "import", "pass", "Falsebreak", "except", "in", "raise", "Noneclass", "finally", "is", "return", "Truecontinue", "for", "lambda", "trydef", "from", "nonlocal", "while"]

parseNumber :: Parser String Int
parseNumber = fmap (read) $
    string "0" <|>
    do 
        let nonzerodigit = choice (char <$> "123456789")
        x <- nonzerodigit
        xs <- many digit
        return (x:xs)
        
(<||>) :: Parser s a -> Parser s b -> Parser s ()
(<||>) a b = (const () <$> a) <|> (const () <$> b)
    
doAct :: Parser String Token
doAct = (skip *> (KeyWord <$> parseKeyWord) <* skip1) <|> 
        (skip *> (Number <$> parseNumber)   <* skip1) <|>
        (skip *> (Ident <$> parseIdent)     <* skip1)
            where
                skip = many $ char ' '
                skip1 = (some $ char ' ') <||> eof
