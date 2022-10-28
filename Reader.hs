module Reader where

import Data.List

type Symbol = String
type Terminal = String
type NonTerminal = String
data Token = Semicolon | AlsoDerives | Epsilon | Derives | Symbol String deriving Show
type Production = (NonTerminal, [Symbol])
data IR = IR [Production] [Terminal] [NonTerminal] deriving Show

tokenize :: String -> Token
tokenize ";" = Semicolon
tokenize "|" = AlsoDerives
tokenize "epsilon" = Epsilon
tokenize "Epsilon" = Epsilon
tokenize "EPSILON" = Epsilon
tokenize ":" = Derives
tokenize s = Symbol s

grammarScan :: String -> [Token]
grammarScan fileString = map tokenize (words fileString)

getSymbols :: [Token] -> [Symbol]
getSymbols token = nub [x | Symbol x <- token]

getTerminals :: [Production] -> [Symbol] -> ([Terminal], [NonTerminal])
getTerminals productions symbols =
    let lhsSymbols = [x | (x, _) <- productions]
        terminals = [x | x <- symbols, not (x `elem` lhsSymbols)]
    in (terminals, lhsSymbols)

parseGrammar :: [Token] -> IR
parseGrammar tokens = 
    let (productions, newTokens) = parseProductionList tokens
        symbols = getSymbols tokens
        (terminals, nonTerminals) = getTerminals productions symbols
    in IR productions (nub terminals) (nub nonTerminals)

parseProductionList :: [Token] -> ([Production], [Token])
parseProductionList tokens = 
    let (rhSide, rest) = parseProductionSet tokens
        (newProductions, newTokens) = parseProductionListPrime rest
        addedSides = rhSide++newProductions
    in (addedSides, newTokens)

parseProductionListPrime :: [Token] -> ([Production], [Token])
parseProductionListPrime [] = ([], [])
parseProductionListPrime tokens = 
    let (owo, uwu) = parseProductionSet tokens
        (guh, gah) = parseProductionListPrime uwu
    in ((owo ++ guh), gah)

parseProductionSet :: [Token] -> ([Production], [Token])
parseProductionSet (Symbol s:Derives:tokens) = 
    let (rightHandSide, rest) = parseRightHandSide tokens
        (rightSide, afterRest) = parseProductionSetPrime rest
        rhsides = rightHandSide:rightSide
        repeatedvalue = repeat s
    in( zip repeatedvalue rhsides, afterRest)

parseProductionSetPrime :: [Token] -> ([[Symbol]], [Token])
parseProductionSetPrime (Semicolon:tokens) = ([], tokens)
parseProductionSetPrime (AlsoDerives:tokens) = 
    let (rightHandSide, rest) = parseRightHandSide tokens
        (rhsides, afterRest) = parseProductionSetPrime rest
    in ((rightHandSide):rhsides, afterRest)

parseRightHandSide :: [Token]  -> ([Symbol], [Token])
parseRightHandSide (Epsilon:tokens) = ([], tokens)
parseRightHandSide tokens@((Symbol s):_) = 
    let (before, after) = parseSymbolList (tokens) 
    in (before, after)

parseSymbolList :: [Token] -> ([Symbol], [Token])
parseSymbolList ((Symbol s):tokens) = 
    let (newSymbols, newTokens) = parseSymbolListPrime tokens
    in  (s:newSymbols, newTokens)

parseSymbolListPrime :: [Token] -> ([Symbol], [Token])
parseSymbolListPrime tokens@(AlsoDerives:_) = ([], tokens)
parseSymbolListPrime tokens@(Semicolon:_) = ([], tokens)
parseSymbolListPrime ((Symbol s):tokens) = 
    let (newSymbols, newTokens) = parseSymbolListPrime tokens
    in  (s:newSymbols, newTokens)
