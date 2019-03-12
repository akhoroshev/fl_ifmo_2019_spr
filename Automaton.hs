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
    let allKeys =
                [ (b, a)
                | b <- Set.toList (states aut)
                , a <- Set.toList (sigma aut)
                ]
    in  foldr (\key b -> checkValues key && b) True allKeys
    where checkValues key = length (Map.lookup key (delta aut)) == 1

-- Checks if the automaton is minimal (only for DFAs: the number of states is minimal)
isMinimal :: Automaton a b -> Bool
isMinimal = undefined

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

-- Thompson algorithm (for automaton without epsilon transitions)
convertNFAtoFDA :: (Ord a, Ord b) => Automaton a b -> Automaton a [b]
convertNFAtoFDA aut = evalState
    (convertNFAtoFDA_ aut)
    (Info Set.empty (Set.fromList [[initState aut]]) Set.empty Map.empty)
  where

    rmdups :: (Ord a) => [a] -> [a]
    rmdups = map head . group . sort

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
                let edgesTo = foldr
                        (\(item, symb) ans ->
                            ( rmdups
                                .   catMaybes
                                .   concat
                                $   (\i -> Map.lookup (i, symb) oldDelta)
                                <$> item
                                , symb
                                )
                                : ans
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
