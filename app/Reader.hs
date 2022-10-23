module Reader where

type Symbol = String
type Terminal = String
type NonTerminal = String
data Token = Semicolon | AlsoDerives | Epsilon | Derives | Symbol String deriving Show
type Production = (NonTerminal, [Symbol])
data IR = IR [Production] [Terminal] [Symbol]


tokenize :: String -> Token
tokenize ";" = Semicolon
tokenize "|" = AlsoDerives
tokenize "epsilon" = Epsilon
tokenize "Epsilon" = Epsilon
tokenize "EPSILON" = Epsilon
tokenize ":" = Derives
tokenize s = Symbol s

getSymbols :: [Token] -> [Symbol] -- Unique only
getSymbols = undefined

grammarScan :: String -> ([Token], [Symbol])
grammarScan fileString = let tokens = map tokenize (words fileString) in (tokens, getSymbols tokens)

parseGrammar :: [Token] -> (IR, [Token])
parseGrammar tokens = 
    let (productionList, newTokens) = parseProductionList tokens
    in undefined

parseProductionList :: [Token] -> ([Production], [Token])
parseProductionList = undefined

parseProductionListPrime :: [Token] -> ([Production], [Token])
parseProductionListPrime = undefined

parseProductionSet :: [Token] -> ([Production], [Token])
parseProductionSet = undefined

parseProductionSetPrime :: [Token] -> ([Production], [Token])
parseProductionSetPrime = undefined

parseRightHandSide :: [Token] -> (Production, [Token])
parseRightHandSide ((Symbol s):tokens) = 
    let (before, after) = parseRightHandSideHelper tokens 
    in ((s, before), after)

parseRightHandSideHelper :: [Token] -> ([Symbol], [Token])
parseRightHandSideHelper (Semicolon:tokens) = ([], tokens)
parseRightHandSideHelper ((Symbol s):tokens) = 
    let (before, after) = parseRightHandSideHelper tokens
    in (s:before, after)

parseSymbolList :: [Token] -> ([Symbol], [Token])
parseSymbolList ((Symbol s):tokens) = 
    let (newSymbols, newTokens) = parseSymbolListPrime tokens
    in  (s:newSymbols, newTokens)

parseSymbolListPrime :: [Token] -> ([Symbol], [Token])

--line 14
parseSymbolListPrime tokens@(AlsoDerives:_) = ([], tokens)
parseSymbolListPrime tokens@(Semicolon:_) = ([], tokens)

--line 13
parseSymbolListPrime ((Symbol s):tokens) = 
    let (newSymbols, newTokens) = parseSymbolListPrime tokens
    in  (s:newSymbols, newTokens)

--IR, symbol table, list of non-terminals You can move the generation of the symbol table to here if you want.
grammarParse :: ([Token], [Symbol]) -> IR
-- grammarParse _ = (parseIR _, parseTerminals _, parseNonTerminals _)
grammarParse = undefined
