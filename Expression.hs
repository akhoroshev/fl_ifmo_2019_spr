module Expression where

import           Control.Applicative
import           Text.Printf
import           Combinators
import           Data.Char
import           Data.Functor


data Operator = Pow
              | Mul
              | Div
              | Sum
              | Minus
              | Eq
              | Neq
              | Le
              | Lt
              | Ge
              | Gt
              | Conj
              | Disj

-- Simplest abstract syntax tree for expressions: only binops are allowed
data EAst a = BinOp Operator (EAst a) (EAst a)
            | Primary a


-- Change the signature if necessary
-- Calculates the value of the input expression
executeExpression :: String -> Either String Integer
executeExpression =
  runParserUntilEof (expression specificationAction parserNum)


parseExpression :: String -> Either String (EAst Integer)
parseExpression =
  runParserUntilEof (expression specificationAST (Primary <$> parserNum))

specificationAST :: [(Assoc, [(Parser Char String, EAst a -> EAst a -> EAst a)])]
specificationAST =
  [ (RAssoc, [(string "||", BinOp Disj)])
  , (RAssoc, [(string "&&", BinOp Conj)])
  , ( NAssoc
    , [ (string "==", BinOp Eq)
      , (string "/=", BinOp Neq)
      , (string "<=", BinOp Le)
      , (string ">=", BinOp Ge)
      , (string "<" , BinOp Lt)
      , (string ">" , BinOp Gt)
      ]
    )
  , (LAssoc, [(string "+", BinOp Sum), (string "-", BinOp Minus)])
  , (LAssoc, [(string "*", BinOp Mul), (string "/", BinOp Div)])
  , (RAssoc, [(string "^", BinOp Pow)])
  ]

specificationAction :: [(Assoc, [(Parser Char String, Integer-> Integer -> Integer)])]
specificationAction =
  let 
    toBool :: Integer -> Bool
    toBool 0 = False
    toBool _ = True

    toInteger :: Bool -> Integer
    toInteger False = 0
    toInteger _ = 1

    argsToBool :: (Bool -> Bool -> Bool) -> Integer -> Integer -> Integer
    argsToBool f x y = toInteger $ f (toBool x) (toBool y)
  in
  [ (RAssoc, [(string "||", argsToBool (||))])
  , (RAssoc, [(string "&&", argsToBool (&&))])
  , ( NAssoc
    , [ (string "==", (toInteger .) <$> (==))
      , (string "/=", (toInteger .) <$> (/=))
      , (string "<=", (toInteger .) <$> (<=))
      , (string ">=", (toInteger .) <$> (>=))
      , (string "<" , (toInteger .) <$> (<))
      , (string ">" , (toInteger .) <$> (>))
      ]
    )
  , (LAssoc, [(string "+", (+)), (string "-", (-))])
  , (LAssoc, [(string "*", (*)), (string "/", div)])
  , (RAssoc, [(string "^", (^))])
  ]

-- Change the signature if necessary
-- Constructs AST for the input expression
parseExpressionDeprecated :: String -> Either String (EAst Integer)
parseExpressionDeprecated input = case runParser parserOr $ streamCreate input of
  Left  err -> Left $ show err
  Right res -> Right $ snd res

instance Show Operator where
  show Pow   = "^"
  show Mul   = "*"
  show Div   = "/"
  show Sum   = "+"
  show Minus = "-"
  show Eq    = "=="
  show Neq   = "/="
  show Le    = "<="
  show Lt    = "<"
  show Ge    = ">="
  show Gt    = ">"
  show Conj  = "&&"
  show Disj  = "||"

instance Show a => Show (EAst a) where
  show = show' 0
   where
    show' n t =
      (if n > 0 then printf "%s|_%s" (concat (replicate (n - 1) "| ")) else id)
        (case t of
          BinOp op l r -> printf "%s\n%s\n%s"
                                 (show op)
                                 (show' (ident n) l)
                                 (show' (ident n) r)
          Primary x -> show x
        )
    ident = (+ 1)

-- Or \to And || Or | And
parserOr :: Parser Char (EAst Integer)
parserOr = parserImpl parserAnd parserOr (string "||" $> Disj)

-- And \to Comp && And | Comp
parserAnd :: Parser Char (EAst Integer)
parserAnd = parserImpl parserComp parserAnd (string "&&" $> Conj)

parserImpl
  :: Parser Char (EAst Integer)
  -> Parser Char (EAst Integer)
  -> Parser Char Operator
  -> Parser Char (EAst Integer)
parserImpl parser1 parser2 parserOp =
  do
      many space
      p1 <- parser1
      many space
      op <- parserOp
      many space
      BinOp op p1 <$> parser2
    <|> many space
    *>  parser1

-- Comp \to Sum Pr Sum | Sum
parserComp :: Parser Char (EAst Integer)
parserComp =
  do
      many space
      s1 <- parserSum
      many space
      pr <- parserPr
      many space
      BinOp pr s1 <$> parserSum
    <|> many space
    *>  parserSum


-- Pr \to == | /= | <= | < | > | >=
parserPr :: Parser Char Operator
parserPr =
  string "=="
    $>  Eq
    <|> string "/="
    $>  Neq
    <|> string "<="
    $>  Le
    <|> string "<"
    $>  Lt
    <|> string ">="
    $>  Ge
    <|> string ">"
    $>  Gt

-- Sum \to Sum + Mul | Sum - Mul | Mul
parserSum :: Parser Char (EAst Integer)
parserSum =
  parserLeftRecImpl parserMul (string "+" $> Sum <|> string "-" $> Minus)

-- Mul \to Mul * Pow | Mul / Pow | Pow
parserMul :: Parser Char (EAst Integer)
parserMul =
  parserLeftRecImpl parserPow (string "*" $> Mul <|> string "/" $> Div)

-- Implement parsing expresions like: A \to A op C | C
parserLeftRecImpl
  :: Parser Char (EAst Integer)
  -> Parser Char Operator
  -> Parser Char (EAst Integer)
parserLeftRecImpl parserA parserOp = do
  first <- many space *> parserA
  other <- many $ do
    op <- many space *> parserOp
    it <- many space *> parserA
    return (op, it)
  return $ foldl (\first (op, it) -> BinOp op first it) first other

-- Pow \to (Or) ^ Pow | Num ^ Pow | (Or) | Num
parserPow :: Parser Char (EAst Integer)
parserPow =
  let alt =
          many space
            *> (   char '('
               *>  many space
               *>  parserOr
               <*  many space
               <*  char ')'
               <|> fmap Primary parserNum
               )
            <* many space
  in  do
          base <- alt
          char '^'
          many space
          BinOp Pow base <$> parserPow
        <|> alt


-- Num \to DigitsNum | Digits | 0
parserNum :: Parser Char Integer
parserNum =
  do
      x  <- parserDigit
      xs <- parserNum
      return (read $ x : show xs)
    <|> toInteger
    .   digitToInt
    <$> parserDigit
    <|> toInteger
    .   digitToInt
    <$> char '0'

-- Digits \to 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
parserDigit :: Parser Char Char
parserDigit = satisfy (\c -> isDigit c && c /= '0')

{-
show (BinOp Conj (BinOp Pow (Primary 1) (BinOp Sum (Primary 2) (Primary 3))) (Primary 4))

&&
|_^
| |_1
| |_+
| | |_2
| | |_3
|_4
-}
