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
    
    Just _ <- runTest "<a>, <1>, <1>, <1>, <(1, a, 1)>"
    Just _ <- runTest "<1>, <q>, <q>, <q>, <(q, 1, q)>"
    Just _ <- runTest "<0, 1>, <A, B, C, D, E, F, G>, <A>, <F, G>, <(A, 0, C), (A, 1, B), (B, 0, C), (B, 1, A), (C, 0, D), (C, 1, D), (D, 0, E), (D, 1, F), (E, 0, F), (E, 1, G), (F, 0, F), (F, 1, F), (G, 0, G), (G, 1, F)>"
        
    Nothing <- runTest "<aa, bb, cc>, <stone, sttwo>, <stone>, <sttwo>, <(stone, ccc, sttwo), (sttwo, bb, stone)>"
    return ()
    
main :: IO ()
main = do
  [fileName] <- getArgs
  input <- readFile fileName
  let a = parseAutomaton input
  putStrLn $ maybe "Not an automaton!" (const "Hurray! Correct automaton!") a
