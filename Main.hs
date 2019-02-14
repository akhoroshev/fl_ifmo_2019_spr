module Main where

import Tokenizer

runTokenizer :: String -> IO [Token]
runTokenizer input = do
  putStrLn $ "Input: " ++ input
  let result = tokenize input
  putStrLn $ "Output: " ++ (show result)
  return result

main :: IO ()
main = do
    [Number 1, Number 2, Ident "abc", KeyWord "if"] <- runTokenizer " 1 2 abc if "
    [Number 1, Number 2, Ident "abcif", KeyWord "if"] <- runTokenizer " 1 2 abcif if "
    [Number 1, Number 2, KeyWord "for", KeyWord "if"] <- runTokenizer " 1 2 for if "
    [Number 0, Number 12] <- runTokenizer " 0 12"
    return ()
