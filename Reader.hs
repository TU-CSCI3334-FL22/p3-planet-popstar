module Reader where

type Symbol = String
type Terminal = String
type NonTerminal = String
data Token = SEMICOLON | ALSODERIVES | EPSILON | DERIVES | SYMBOL String deriving Show
type Production = (NonTerminal, [Symbol])
data IR = IR [Production] [Terminal] [Symbol]


tokenize :: String -> Token
tokenize ";" = SEMICOLON
tokenize "|" = ALSODERIVES
tokenize "epsilon" = EPSILON
tokenize "Epsilon" = EPSILON
tokenize "EPSILON" = EPSILON
tokenize ":" = DERIVES
tokenize s = SYMBOL s

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

parseSymbolList :: [Token] -> ([Symbol], [Token])
parseSymbolList ((SYMBOL s):tokens) = 
    let (newSymbols, newTokens) = parseSymbolListPrime tokens
    in  (s:newSymbols, newTokens)

parseSymbolListPrime :: [Token] -> ([Symbol], [Token])
--line 14
parseSymbolListPrime tokens@(ALSODERIVES:_) = ([], tokens)
parseSymbolListPrime tokens@(SEMICOLON:_) = ([], tokens)
--line 13
parseSymbolListPrime ((SYMBOL s):tokens) = 
    let (newSymbols, newTokens) = parseSymbolListPrime tokens
    in  (s:newSymbols, newTokens)

--IR, symbol table, list of non-terminals You can move the generation of the symbol table to here if you want.
grammarParse :: ([Token], [Symbol]) -> IR
-- grammarParse _ = (parseIR _, parseTerminals _, parseNonTerminals _)
grammarParse = undefined
