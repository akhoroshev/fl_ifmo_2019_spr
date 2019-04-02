module Test where

import           Automaton
import           Combinators

main :: IO ()
main = do
    testParseAutomaton
    testIsDFA
    testIsComplete
    testCompletionDFA
    testIsMinimal
    testMinimalization
    testConvertNFAtoDFA
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
    Right _ <- runTestParse "<aa, bb, cc>, <stone, sttwo>, <stone>, <sttwo>, <(stone, aa, sttwo)>"
    Right _ <- runTestParse "<aa, bb, cc>, <stone, sttwo>, <stone>, <sttwo>, <(stone, bb, sttwo)>"
    Right _ <- runTestParse "<aa, bb, cc>, <stone, sttwo>, <stone>, <sttwo>, <(stone, cc, sttwo), (sttwo, bb, stone)>"
    Right _ <- runTestParse "<aa, bb, cc>, <stone, sttwo>, <stone>, <>, <>"

    Right _ <- runTestParse "<a>, <1>, <1>, <1>, <(1, a, 1)>"
    Right _ <- runTestParse "<1>, <q>, <q>, <q>, <(q, 1, q)>"
    Right _ <- runTestParse "<0, 1>, <A, B, C, D, E, F, G>, <A>, <F, G>, <(A, 0, C), (A, 1, B), (B, 0, C), (B, 1, A), (C, 0, D), (C, 1, D), (D, 0, E), (D, 1, F), (E, 0, F), (E, 1, G), (F, 0, F), (F, 1, F), (G, 0, G), (G, 1, F)>"

    Left _ <- runTestParse "<aa, bb, cc>, <stone, sttwo>, <stone>, <sttwo>, <(stone, ccc, sttwo), (sttwo, bb, stone)>"

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
    Right True <- pure $ isDFA <$> parseAutomaton "<a>, <1>, <1>, <1>, <>"
    Right True <- pure $ isDFA <$> parseAutomaton "<a,b>, <1,2>, <1>, <1>, <(1, a, 2), (2, a, 1)>"

    Right False <- pure $ isDFA <$> parseAutomaton "<a>, <1>, <1>, <1>, <(1, \\epsilon, 1)>"
    Right False <- pure $ isDFA <$> parseAutomaton "<a,b>, <1,2>, <1>, <1>, <(1, a, 1), (1, a, 2)>"
    Right False <- pure $ isDFA <$> parseAutomaton "<a>, <1>, <1>, <1>, <(1, \\epsilon, 1)>"
    Right False <- pure $ isDFA <$> parseAutomaton "<a>, <1>, <1>, <1>, <(1, a, 1), (1, \\epsilon, 1)>"
    Right False <- pure $ isDFA <$> parseAutomaton "<a,b>, <1,2>, <1>, <1>, <(1, a, 1), (2, b, 1), (2, b, 2), (1, \\epsilon, 1)>"
    Right False <- pure $ isDFA <$> parseAutomaton "<a,b,c>, <1,2>, <1>, <1>, <(1, a, 1), (1, b, 1), (1, b, 2)>"
    Right False <- pure $ isDFA <$> parseAutomaton "<a, b>, <1, 2, 3>, <1>, <1,3>,<(1, a, 1), (2, b, 1), (1, b, 2), (2, b, 2), (2, a, 3), (3, a, 2), (3, b, 3)>"
    return ()

testIsComplete :: IO ()
testIsComplete = do
    Right True <- pure $ isComplete <$> parseAutomaton "<a>, <1>, <1>, <1>, <(1, a, 1)>"
    Right True <- pure $ isComplete <$> parseAutomaton "<a,b>, <1>, <1>, <1>, <(1, a, 1), (1, b, 1)>"
    Right True <- pure $ isComplete <$> parseAutomaton "<a,b>, <1,2>, <1>, <1>, <(1, a, 1), (1, b, 1), (2, a, 1), (2, b, 1)>"

    Right False <- pure $ isComplete <$> parseAutomaton "<a>, <1>, <1>, <1>, <>"
    Right False <- pure $ isComplete <$> parseAutomaton "<a,b>, <1>, <1>, <1>, <(1, a, 1)>"
    Right False <- pure $ isComplete <$> parseAutomaton "<a,b>, <1,2>, <1>, <1>, <(1, a, 1), (1, b, 1), (2, a, 1)>"
    Right False <- pure $ isComplete <$> parseAutomaton "<a, b>, <1, 2, 3>, <1>, <1,3>,<(1, a, 1), (2, b, 1), (1, b, 2), (2, b, 2), (2, a, 3), (3, a, 2), (3, b, 3)>"
    return ()

testCompletionDFA :: IO ()
testCompletionDFA = do
    Right True <- pure $ isComplete . completionDFA <$> parseAutomaton "<a>, <1>, <1>, <1>, <(1, a, 1)>"
    Right True <- pure $ isComplete . completionDFA <$> parseAutomaton "<a,b>, <1>, <1>, <1>, <(1, a, 1), (1, b, 1)>"
    Right True <- pure $ isComplete . completionDFA <$> parseAutomaton "<a,b>, <1,2>, <1>, <1>, <(1, a, 1), (1, b, 1), (2, a, 1), (2, b, 1)>"

    Right True <- pure $ isComplete . completionDFA <$> parseAutomaton "<a>, <1>, <1>, <1>, <>"
    Right True <- pure $ isComplete . completionDFA <$> parseAutomaton "<a>, <1, 2>, <1>, <2>, <(1, a, 2)>"
    Right True <- pure $ isComplete . completionDFA <$> parseAutomaton "<a,b>, <1>, <1>, <1>, <(1, a, 1)>"
    Right True <- pure $ isComplete . completionDFA <$> parseAutomaton "<a,b>, <1,2>, <1>, <1>, <(1, a, 1), (1, b, 1), (2, a, 1)>"

    Right False <- pure $ isComplete . completionDFA <$> parseAutomaton "<a, b>, <1, 2, 3>, <1>, <1,3>,<(1, a, 1), (2, b, 1), (1, b, 2), (2, b, 2), (2, a, 3), (3, a, 2), (3, b, 3)>"
    return ()

testIsMinimal :: IO ()
testIsMinimal = do
    Right True <- pure $ isMinimal <$> parseAutomaton "<a>, <1>, <1>, <1>, <(1, a, 1)>"

    Right True <- pure $ isMinimal <$> parseAutomaton "<a>, <1, 2>, <1>, <2>, <(1, a, 2)>"

    Right False <- pure $ isMinimal <$> parseAutomaton "<0,1>,<a,b,c,d,e,f,g>,<a>,<f,g>,<(a,0,c),(a,1,b),(b,0,c),(b,1,a),(c,0,d),(c,1,d),(d,0,e),(d,1,f),(e,1,g),(e,0,f),(f,0,f),(f,1,f),(g,0,g),(g,1,f)>"
    return ()

testMinimalization :: IO ()
testMinimalization = do
    let expected = "Automaton {sigma = fromList [\"0\",\"1\"], states = fromList [[\"a\",\"b\"],[\"c\"],[\"d\"],[\"e\"],[\"f\",\"g\"]], initState = [\"a\",\"b\"], termState = fromList [[\"f\",\"g\"]], delta = [(([\"a\",\"b\"],\"0\"),Just [\"c\"]),(([\"a\",\"b\"],\"1\"),Just [\"a\",\"b\"]),(([\"c\"],\"0\"),Just [\"d\"]),(([\"c\"],\"1\"),Just [\"d\"]),(([\"d\"],\"0\"),Just [\"e\"]),(([\"d\"],\"1\"),Just [\"f\",\"g\"]),(([\"e\"],\"0\"),Just [\"f\",\"g\"]),(([\"e\"],\"1\"),Just [\"f\",\"g\"]),(([\"f\",\"g\"],\"0\"),Just [\"f\",\"g\"]),(([\"f\",\"g\"],\"1\"),Just [\"f\",\"g\"])], epsilon = \"\\\\epsilon\"}"
    Right actual <- pure $ show . minimalizationDFA <$> parseAutomaton "<0,1>,<a,b,c,d,e,f,g>,<a>,<f,g>,<(a,0,c),(a,1,b),(b,0,c),(b,1,a),(c,0,d),(c,1,d),(d,0,e),(d,1,f),(e,1,g),(e,0,f),(f,0,f),(f,1,f),(g,0,g),(g,1,f)>"
    True <- pure (expected == actual)

    let expected = "Automaton {sigma = fromList [\"a\"], states = fromList [[\"1\"]], initState = [\"1\"], termState = fromList [[\"1\"]], delta = [(([\"1\"],\"a\"),Just [\"1\"])], epsilon = \"\\\\epsilon\"}"
    Right actual <- pure $ show . minimalizationDFA <$> parseAutomaton "<a>, <1>, <1>, <1>, <(1, a, 1)>"
    True <- pure (expected == actual)

    let expected = "Automaton {sigma = fromList [\"a\"], states = fromList [[\"1\"],[\"2\"]], initState = [\"1\"], termState = fromList [[\"2\"]], delta = [(([\"1\"],\"a\"),Just [\"2\"])], epsilon = \"\\\\epsilon\"}"
    Right actual <- pure $ show . minimalizationDFA <$> parseAutomaton "<a>, <1, 2>, <1>, <2>, <(1, a, 2)>"
    True <- pure (expected == actual)
    return ()

testConvertNFAtoDFA :: IO ()
testConvertNFAtoDFA = do
    -- https://neerc.ifmo.ru/wiki/index.php?title=Построение_по_НКА_эквивалентного_ДКА,_алгоритм_Томпсона#.D0.9F.D1.80.D0.B8.D0.BC.D0.B5.D1.80
    let expected = "Automaton {sigma = fromList [\"a\",\"b\"], states = fromList [[\"1\"],[\"1\",\"2\"]], initState = [\"1\"], termState = fromList [[\"1\",\"2\"]], delta = [(([\"1\"],\"a\"),Just [\"1\",\"2\"]),(([\"1\"],\"b\"),Just [\"1\"]),(([\"1\",\"2\"],\"a\"),Just [\"1\",\"2\"]),(([\"1\",\"2\"],\"b\"),Just [\"1\",\"2\"])], epsilon = \"\\\\epsilon\"}"
    Right actual <- pure $ show . convertNFAtoDFA <$> parseAutomaton "<a,b>, <1,2>, <1>, <2>, <(1,a,1), (1,b,1), (1,a,2), (2,b,1), (2,b,2)>"
    True <- pure (expected == actual)
    return ()
