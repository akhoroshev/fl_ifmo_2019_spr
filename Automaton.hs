{-# LANGUAGE FlexibleContexts #-}
{-# Language InstanceSigs #-}

module Automaton where

import qualified Data.MultiMap                 as Map
import qualified Data.Set                      as Set

import           Data.List
import           Control.Monad.State.Lazy
import           Combinators
import           Control.Applicative            ( (<|>) )
import           Data.Maybe

type Set = Set.Set
type Map = Map.MultiMap

data Automaton s q = Automaton { sigma     :: Set s
                               , states    :: Set q
                               , initState :: q
                               , termState :: Set q
                               , delta     :: Map (q, s) (Maybe q)
                               , epsilon   :: s
                               } deriving Show

instance (Show k, Show v) => Show (Map.MultiMap k v) where
    show :: Map.MultiMap k v -> String
    show = show . Map.toList

-- Top level function: parses input string, checks that it is an automaton, and then returns it.
-- Should return Nothing, if there is a syntax error or the automaton is not a correct automaton.
-- This includes:
-- * The set of states is empty
-- * The init state is not a state
-- * Any of the terminal states is not a state
-- * Delta function is defined on not-a-state or not-a-symbol-from-sigma
-- Pick appropriate types for s and q
parseAutomaton :: String -> Either String (Automaton String String)
parseAutomaton s = case (runParser prsAutomation (streamCreate s)) of
    Right x -> Right $ snd x
    Left  x -> Left $ show x

prsAutomation :: Parser Char (Automaton String String)
prsAutomation = do
    let epsilon        = "\\epsilon"

    let nonEmptyString = some (char '\\' <|> letter <|> digit)
    let colon          = many space *> char ',' <* many space

    listSigma <- parseList nonEmptyString (char ',') (char '<') (char '>') (> 0)
    let sigma = Set.fromList listSigma

    colon

    listStates <- parseList nonEmptyString
                            (char ',')
                            (char '<')
                            (char '>')
                            (> 0)
    let states = Set.fromList listStates

    colon

    let allowedStates = choice (string <$> listStates)
    [initState] <- parseList allowedStates
                             (char ',')
                             (char '<')
                             (char '>')
                             (== 1)

    colon

    listTermStates <- parseList allowedStates
                                (char ',')
                                (char '<')
                                (char '>')
                                (const True)
    let termState = Set.fromList listTermStates

    colon

    let tripleParser = do
            [fromState, symb, toState] <- parseList nonEmptyString
                                                    (char ',')
                                                    (char '(')
                                                    (char ')')
                                                    (== 3)
            if Set.member fromState states
                   && Set.member toState states
                   && (Set.member symb sigma || symb == epsilon)
                then return ((fromState, symb), Just toState)
                else
                    fail
                    $  "wrong transition: ("
                    ++ show fromState
                    ++ ", "
                    ++ show symb
                    ++ ", "
                    ++ show toState
                    ++ ")"

    listDelta <- parseList tripleParser
                           (char ',')
                           (char '<')
                           (char '>')
                           (const True)
    many space
    eof
    let delta = Map.fromList listDelta

    return $ Automaton sigma states initState termState delta epsilon

-- Checks if the automaton is deterministic (only one transition for each state and each input symbol)
isDFA :: (Ord a, Ord b) => Automaton a b -> Bool
isDFA aut =
    let keys = Map.keys $ delta aut
    in  checKeys keys && foldr (\key b -> checkValues key && b) True keys
  where
    checkValues key = length (Map.lookup key (delta aut)) <= 1
    checKeys keys = epsilon aut `notElem` (snd <$> keys)


-- Checks if the automaton is nondeterministic (eps-transition or multiple transitions for a state and a symbol)
isNFA :: Automaton a b -> Bool
isNFA = const True

-- Checks if the automaton is complete (there exists a transition for each state and each input symbol)
isComplete :: (Ord a, Ord b) => Automaton a b -> Bool
isComplete aut =
    isDFA aut
        && let allKeys =
                   [ (b, a)
                   | b <- Set.toList (states aut)
                   , a <- Set.toList (sigma aut)
                   ]
           in  foldr (\key b -> checkValues key && b) True allKeys
    where checkValues key = length (Map.lookup key (delta aut)) == 1

-- Checks if the automaton is minimal (only for DFAs: the number of states is minimal)
isMinimal :: (Ord a, Ord b) => Automaton a b -> Bool
isMinimal aut =
    isDFA aut
        && Set.size (states $ minimalizationDFA (completionDFA aut))
        == Set.size (states aut)

-- Transform non complete DFA to complete DFA
completionDFA :: (Ord a, Ord b) => Automaton a b -> Automaton a b
completionDFA aut = aut { delta = updated }
  where
    allKeys =
        [ (b, a) | b <- Set.toList (states aut), a <- Set.toList (sigma aut) ]
    updated = foldr
        (\key delta' -> if Map.notMember delta' key
            then Map.insert key Nothing delta'
            else delta'
        )
        (delta aut)
        allKeys


data ConvertionState a b = Info { added' :: Set [b]
                                , queue' :: Set [b]
                                , term' :: Set [b]
                                , delta' :: Map ([b], a) (Maybe [b])
                                }
-- remove copies and sort
rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

-- Thompson algorithm (for automaton without epsilon transitions)
convertNFAtoDFA :: (Ord a, Ord b) => Automaton a b -> Automaton a [b]
convertNFAtoDFA aut = if epsilon aut `elem` (snd <$> (Map.keys $ delta aut))
    then error "Automaton without eplsilon transition expected"
    else evalState
        (convertNFAtoFDA_ aut)
        (Info Set.empty (Set.fromList [[initState aut]]) Set.empty Map.empty)
  where
    convertNFAtoFDA_
        :: (Ord a, Ord b)
        => Automaton a b
        -> State (ConvertionState a b) (Automaton a [b])
    convertNFAtoFDA_ aut = do
        let oldTerminal = termState aut
        let oldDelta    = delta aut
        let oldSigma    = sigma aut
        q <- gets queue'
        if null q
            then do
                newDelta    <- gets delta'
                newStates   <- gets added'
                newTerminal <- gets term'
                return $ Automaton (sigma aut)
                                   newStates
                                   [initState aut]
                                   newTerminal
                                   newDelta
                                   (epsilon aut)
            else do
                itemList <- gets $ Set.elemAt 0 . queue'
                modify (\st -> st { queue' = Set.deleteAt 0 (queue' st) })
                modify (\st -> st { added' = Set.insert itemList (added' st) })

                when (or $ flip Set.member oldTerminal <$> itemList) $ modify
                    (\st -> st { term' = Set.insert itemList (term' st) })

                let forCheck = [ (itemList, a) | a <- Set.toList oldSigma ]
                let
                    edgesTo = filter (not . null . fst) $ foldr
                        (\(item, symb) ans ->
                            let edges =
                                        ( rmdups
                                            .   catMaybes
                                            .   concat
                                            $ (\i -> Map.lookup (i, symb) oldDelta)
                                            <$> item
                                        , symb
                                        )
                            in  edges : ans
                        )
                        []
                        forCheck

                let
                    processEdge (xs, a) = do
                        alreadyAdded <- gets $ Set.member xs . added'
                        modify
                            (\st -> st
                                { delta' = Map.insert (itemList, a)
                                                      (Just xs)
                                                      (delta' st)
                                }
                            )
                        unless alreadyAdded
                            $ modify
                                  (\st -> st
                                      { queue' = Set.insert xs (queue' st)
                                      }
                                  )

                mapM_ processEdge edgesTo

                convertNFAtoFDA_ aut

-- Epsilon closure
epsilonClosure :: (Ord a, Ord b) => Automaton a b -> Automaton a [b]
epsilonClosure aut = Automaton oldSigma
                               (Set.fromList newStates)
                               newInitState
                               newTerminal
                               newDelta
                               oldEplsilon
  where
    oldEplsilon  = epsilon aut
    oldStates    = states aut
    oldSigma     = sigma aut
    oldDelta     = delta aut
    oldTerminal  = termState aut
    oldInitState = initState aut
    --dfs :: [b] -> b -> [b]
    dfs taken node
        | node `elem` taken
        = taken
        | otherwise
        = foldl dfs (node : taken)
            $            catMaybes
            $            (node, oldEplsilon)
            `Map.lookup` oldDelta

    statesToFilter = Set.fromList . dfs [] <$> Set.toList oldStates
    newStates      = Set.toList <$> rmdups
        (filter (\x -> all (not . Set.isProperSubsetOf x) statesToFilter)
                statesToFilter
        )

    newDelta = Map.fromList $ do
        sigma  <- Set.toList oldSigma
        states <- newStates
        let oldDest =
                rmdups
                    $ catMaybes
                          (concatMap
                              (\st -> Map.lookup (st, sigma) oldDelta)
                              states
                          )
            newDest =
                rmdups $ concatMap (\x -> filter (elem x) newStates) oldDest
        dest <- newDest
        return ((states, sigma), Just dest)

    newTerminal = foldl
        (\set x ->
            if any (`elem` oldTerminal) x then x `Set.insert` set else set
        )
        Set.empty
        newStates
    newInitState = fromMaybe (error "init not found")
        $ find (elem oldInitState) newStates


-- Minimalization algorithm
minimalizationDFA :: (Ord a, Ord b) => Automaton a b -> Automaton a [b]
minimalizationDFA aut' = if not $ isDFA aut'
    then error "Automaton must be a DFA"
    else
        let
            aut        = completionDFA aut'
            allStates  = Set.toList $ states aut
            termStates = Set.toList $ termState aut
            nonTerm    = allStates \\ termStates
            initQueue =
                [ (min term all, max term all)
                | term <- termStates
                , all  <- nonTerm
                ]
            invertedDelta = invertDelta aut
            joinedClasses = joinState (Set.fromList initQueue)
                                      (sigma aut)
                                      (states aut)
                                      invertedDelta
            newTerminals = markTerminal (termState aut) joinedClasses
            newStart     = markStart (initState aut) joinedClasses
            newDelta     = markDelta (delta aut) (sigma aut) joinedClasses
        in
            Automaton (sigma aut)
                      (Set.fromList joinedClasses)
                      newStart
                      newTerminals
                      newDelta
                      (epsilon aut)
  where
    markTerminal :: Ord b => Set b -> [[b]] -> Set [b]
    markTerminal terminal states =
        Set.filter (any (`Set.member` terminal)) (Set.fromList states)

    markStart :: Ord b => b -> [[b]] -> [b]
    markStart start states = head $ filter (elem start) states

    markDelta
        :: (Ord b, Ord a)
        => Map (b, a) (Maybe b)
        -> Set a
        -> [[b]]
        -> Map ([b], a) (Maybe [b])
    markDelta delta sigma states = Map.fromList
        [ ((st1, sig), Just st2)
        | sig <- Set.toList sigma
        , st1 <- states
        , st2 <- states
        , or
            $   pure (\x y -> Map.lookup (x, sig) delta == [Just y])
            <*> st1
            <*> st2
        ]


-- Reverse delta function (only for complete DFA)
invertDelta :: (Ord a, Ord b) => Automaton a b -> Map (b, a) b
invertDelta aut = execState (invertDeltaImpl aut) Map.empty
  where
    invertDeltaImpl
        :: (Ord a, Ord b) => Automaton a b -> State (Map (b, a) b) ()
    invertDeltaImpl aut = do
        let deltaFun = delta aut
        let allSigma = Set.toList $ sigma aut
        let allState = Set.toList $ states aut

        let
            processEdge (currentSigma, currentState) = do
                let [targetState] =
                        Map.lookup (currentState, currentSigma) deltaFun
                unless (isNothing targetState)
                    $ modify
                          (Map.insert (fromJust targetState, currentSigma)
                                      currentState
                          )

        mapM_ processEdge [ (sig, st) | sig <- allSigma, st <- allState ]

-- Join equal state in DFA (only for complete DFA)
joinState
    :: (Ord a, Ord b) => Set (b, b) -> Set a -> Set b -> Map (b, a) b -> [[b]]
joinState queue sigma states delta = evalState
    (splitClassesImpl sigma states delta)
    (queue, queue)
  where
    splitClassesImpl
        :: (Ord a, Ord b)
        => Set a
        -> Set b
        -> Map (b, a) b
        -> State (Set (b, b), Set (b, b)) [[b]]
    splitClassesImpl sigma states delta = do
        let modifyQueue f = modify $ \(queue, table) -> (f queue, table)
        let modifyTable f = modify $ \(queue, table) -> (queue, f table)
        let getsQueue f = gets $ \(queue, _) -> f queue
        let getsTable f = gets $ \(_, table) -> f table

        queue <- getsQueue id
        if null queue
            then do
                finalTable <- getsTable id
                let classes =
                        rmdups
                            $   rmdups
                            <$> [ [ st2
                                  | st2 <- Set.toList states
                                  , not $ Set.member
                                      (min st1 st2, max st1 st2)
                                      finalTable
                                  ]
                                | st1 <- Set.toList states
                                ]
                return classes
            else do
                (st1', st2') <- getsQueue $ Set.elemAt 0
                modifyQueue $ Set.deleteAt 0
                let st1 = min st1' st2'
                    st2 = max st1' st2'
                let uniqToProcess = rmdups
                        [ (min b1 b2, max b1 b2)
                        | sig <- Set.toList sigma
                        , b1  <- Map.lookup (st1, sig) delta
                        , b2  <- Map.lookup (st2, sig) delta
                        ]

                let
                    processPair pair = do
                        inTable <- getsTable $ Set.member pair
                        unless inTable
                            $  modifyQueue (Set.insert pair)
                            >> modifyTable (Set.insert pair)

                mapM_ processPair uniqToProcess
                splitClassesImpl sigma states delta
