module Reader where

type Symbol = String
type Terminal = String
type NonTerminal = String
data Token = Semicolon | AlsoDerives | Epsilon | Derives | Symbol String deriving Show
type Production = (NonTerminal, [Token])
data IR = IR [Production] [Terminal] [Symbol]

tokenList :: [Token]
-- tokenList = [Symbol "List", Derives, Symbol "Pair", Symbol "List", AlsoDerives, Epsilon, Semicolon, Symbol "Pair", Derives, Symbol "LParen", Symbol "List", Symbol "RParen", Semicolon]
-- tokenList = [Symbol "List", Derives, Symbol "Pair", Symbol "List", Semicolon, Symbol "Pair", Derives, Symbol "LParen", Symbol "List", Symbol "RParen", Semicolon]
-- tokenList = [AlsoDerives, Symbol "Pair", Semicolon, Symbol "Pair", Derives, Symbol "LParen", Symbol "List", Symbol "RParen", Semicolon]
-- tokenList = [Symbol "List", Symbol "Pair", Symbol "List", Semicolon, Symbol "Pair",  Symbol "LParen", Symbol "List", Symbol "RParen", Semicolon]
tokenList = [Symbol "Pair", Symbol "List", Semicolon]
epsilonList = [Epsilon, Semicolon]
prodList = [Symbol "List", Derives, Epsilon, AlsoDerives, Symbol "List", Semicolon, Symbol "Pair", Derives, Symbol "LParen", Semicolon]

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
parseProductionListPrime [] = ([], []) -- eof case? 
parseProductionListPrime tokens = 
    let (owo, uwu) = parseProductionSet tokens -- brings the first production and the rest following token
        (guh, gah) = parseProductionListPrime uwu
    in ((owo ++ guh), gah)


-- line 6
parseProductionSet :: [Token] -> ([Production], [Token])
parseProductionSet (Symbol s:Derives:tokens) = 
    let (rightHandSide, rest) = parseRightHandSide tokens
        (rightSide, afterRest) = parseProductionSetPrime rest
        rhsides = rightHandSide:rightSide
        repeatedvalue = repeat s
    in ((zip repeatedvalue rhsides), afterRest) --([Symbol s, rhs], afterRest)

-- line 7
parseProductionSetPrime :: [Token] -> ([[Token]], [Token])
parseProductionSetPrime (Semicolon:tokens) = ([], tokens)
parseProductionSetPrime (AlsoDerives:tokens) = 
    let (rightHandSide, rest) = parseRightHandSide tokens
        (rhsides, afterRest) = parseProductionSetPrime rest
    in ((rightHandSide):rhsides, afterRest)

parseRightHandSide :: [Token]  -> ([Token], [Token]) --([Tokens on right hand side], [Tokens remaining])
parseRightHandSide (Epsilon:tokens) = ([Epsilon], tokens)
parseRightHandSide tokens@((Symbol s):_) = 
    let (before, after) = parseSymbolList (tokens) 
    in (before, after)

parseSymbolList :: [Token] -> ([Token], [Token])
parseSymbolList ((Symbol s):tokens) = 
    let (newSymbols, newTokens) = parseSymbolListPrime tokens
    in  (Symbol s:newSymbols, newTokens)

parseSymbolListPrime :: [Token] -> ([Token], [Token])

--line 14
parseSymbolListPrime tokens@(AlsoDerives:_) = ([], tokens)
parseSymbolListPrime tokens@(Semicolon:_) = ([], tokens)

--line 13
parseSymbolListPrime ((Symbol s):tokens) = 
    let (newSymbols, newTokens) = parseSymbolListPrime tokens
    in  (Symbol s:newSymbols, newTokens)
parseSymbolListPrime [] = ([], []) -- shouldn't happen

--IR, symbol table, list of non-terminals You can move the generation of the symbol table to here if you want.
grammarParse :: ([Token], [Symbol]) -> IR
-- grammarParse _ = (parseIR _, parseTerminals _, parseNonTerminals _)
grammarParse = undefined
