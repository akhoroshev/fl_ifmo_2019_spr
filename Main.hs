module Main where

import System.Environment
import Automaton

runTest :: String -> IO (Maybe (Automaton String String))
runTest input = do
    putStrLn $ "Input: " ++ input
    let result = parseAutomaton input
    putStrLn $ "Output: " ++ (show result)
    return result

test :: IO ()
test = do
    Just _ <- runTest "<aa, bb, cc>, <stone, sttwo>, <stone>, <sttwo>, <(stone, aa, sttwo)>"
    Just _ <- runTest "<aa, bb, cc>, <stone, sttwo>, <stone>, <sttwo>, <(stone, bb, sttwo)>"
    Just _ <- runTest "<aa, bb, cc>, <stone, sttwo>, <stone>, <sttwo>, <(stone, cc, sttwo), (sttwo, bb, stone)>"
    Just _ <- runTest "<aa, bb, cc>, <stone, sttwo>, <stone>, <>, <>"
    
    Nothing <- runTest "<aa, bb, cc>, <stone, sttwo>, <stone>, <sttwo>, <(stone, ccc, sttwo), (sttwo, bb, stone)>"
    return ()
    
main :: IO ()
main = do
  [fileName] <- getArgs
  input <- readFile fileName
  let a = parseAutomaton input
  putStrLn $ maybe "Not an automaton!" (const "Hurray! Correct automaton!") a
