module Automaton where

import Combinators

import Prelude hiding (fail)

import Control.Applicative ((<|>))

import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set

type Set = Set.Set
type Map = Map.Map

data Automaton s q = Automaton { sigma     :: Set s
                               , states    :: Set q
                               , initState :: q
                               , termState :: Set q
                               , delta     :: Map (q, s) (Maybe q)
                               } deriving Show

-- Top level function: parses input string, checks that it is an automaton, and then returns it.
-- Should return Nothing, if there is a syntax error or the automaton is not a correct automaton.
-- This includes:
-- * The set of states is empty
-- * The init state is not a state
-- * Any of the terminal states is not a state
-- * Delta function is defined on not-a-state or not-a-symbol-from-sigma
-- Pick appropriate types for s and q
parseAutomaton :: String -> Maybe (Automaton String String)
parseAutomaton s = snd <$> (runParser prsAutomation s)

prsAutomation :: Parser String (Automaton String String)
prsAutomation = do
    let nonEmptyString = some letter
    let colon = (many space) *> char ',' <* (many space)
    
    listSigma <- parseList nonEmptyString (char ',') (char '<') (char '>') (>0)
    let sigma = Set.fromList listSigma
    colon
    
    listStates <- parseList nonEmptyString (char ',') (char '<') (char '>') (>0)
    let states= Set.fromList listStates
    colon
    
    let allowedStates = choice (string <$> listStates)
    [ initState ] <- parseList allowedStates (char ',') (char '<') (char '>') (==1)
        
    colon
    
    listTermStates <- parseList allowedStates (char ',') (char '<') (char '>') (>0)
    let termState = Set.fromList listTermStates
    colon
    
    let tripleParser = do
        [fromState, symb, toState] <- parseList nonEmptyString (char ',') (char '(') (char ')') (==3)
        return ((fromState, symb), Just toState)
    
    listDelta <- parseList tripleParser (char ',') (char '<') (char '>') (const True)
    let delta = Map.fromList listDelta
    
    return $ Automaton sigma states initState termState delta
