module Test where

import           Automaton
import           Combinators

main :: IO ()
main = do
    testParseAutomaton
    testIsDFA
    testIsComplete
    putStrLn "Test passed"
    return ()


runTestParse :: String -> IO (Either String (Automaton String String))
runTestParse input = do
    putStrLn $ "Input: " ++ input
    let result = parseAutomaton input
    putStrLn $ "Output: " ++ show result
    return result

testParseAutomaton :: IO ()
testParseAutomaton = do
    Right _ <- runTestParse
        "<aa, bb, cc>, <stone, sttwo>, <stone>, <sttwo>, <(stone, aa, sttwo)>"
    Right _ <- runTestParse
        "<aa, bb, cc>, <stone, sttwo>, <stone>, <sttwo>, <(stone, bb, sttwo)>"
    Right _ <-
        runTestParse
            "<aa, bb, cc>, <stone, sttwo>, <stone>, <sttwo>, <(stone, cc, sttwo), (sttwo, bb, stone)>"
    Right _ <- runTestParse "<aa, bb, cc>, <stone, sttwo>, <stone>, <>, <>"

    Right _ <- runTestParse "<a>, <1>, <1>, <1>, <(1, a, 1)>"
    Right _ <- runTestParse "<1>, <q>, <q>, <q>, <(q, 1, q)>"
    Right _ <-
        runTestParse
            "<0, 1>, <A, B, C, D, E, F, G>, <A>, <F, G>, <(A, 0, C), (A, 1, B), (B, 0, C), (B, 1, A), (C, 0, D), (C, 1, D), (D, 0, E), (D, 1, F), (E, 0, F), (E, 1, G), (F, 0, F), (F, 1, F), (G, 0, G), (G, 1, F)>"

    Left _ <-
        runTestParse
            "<aa, bb, cc>, <stone, sttwo>, <stone>, <sttwo>, <(stone, ccc, sttwo), (sttwo, bb, stone)>"

    Left  _ <- runTestParse "<a>, <b>, <b>, <b>, <(b, a, b)> , <a>"
    Left  _ <- runTestParse "<a>, <b>, <b>, <b>, <(b, a, b)> ,"
    Right _ <- runTestParse "<a>, <b>, <b>, <b>, <(b, a, b)>      "
    return ()

testIsDFA :: IO ()
testIsDFA = do
    Right True <- pure $ isDFA <$> parseAutomaton "<a>, <1>, <1>, <1>, <(1, a, 1)>"
    Right True <- pure $ isDFA <$> parseAutomaton "<a,b>, <1,2>, <1>, <1>, <(1, a, 1)>"
    Right True <- pure $ isDFA <$> parseAutomaton "<a,b,c>, <1,2>, <1>, <1>, <(1, a, 1), (1, b, 1)>"
    Right True <- pure $ isDFA <$> parseAutomaton "<a,b,c>, <1,2>, <1>, <1>, <(1, a, 1), (1, b, 1), (1, c, 1)>"

    Right False <- pure $ isDFA <$> parseAutomaton "<a>, <1>, <1>, <1>, <(1, epsilon, 1)>"
    Right False <- pure $ isDFA <$> parseAutomaton "<a,b>, <1,2>, <1>, <1>, <(1, a, 1), (1, a, 2)>"
    return ()


testIsComplete :: IO ()
testIsComplete = do
    Right True <- pure $ isComplete <$> parseAutomaton "<a>, <1>, <1>, <1>, <(1, a, 1)>"
    Right True <- pure $ isComplete <$> parseAutomaton "<a,b>, <1>, <1>, <1>, <(1, a, 1), (1, b, 1)>"
    Right True <- pure $ isComplete <$> parseAutomaton "<a,b>, <1,2>, <1>, <1>, <(1, a, 1), (1, b, 1), (2, a, 1), (2, b, 1)>"


    Right False <- pure $ isComplete <$> parseAutomaton "<a>, <1>, <1>, <1>, <>"
    Right False <- pure $ isComplete <$> parseAutomaton "<a,b>, <1>, <1>, <1>, <(1, a, 1)>"
    Right False <- pure $ isComplete <$> parseAutomaton "<a,b>, <1,2>, <1>, <1>, <(1, a, 1), (1, b, 1), (2, a, 1)>"
    return ()