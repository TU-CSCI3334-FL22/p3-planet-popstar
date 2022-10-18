module Reader where

type Symbol = String
type Terminal = String
type NonTerminal = String
-- type SymbolTable = [(String, Int)] --Map of strings  to ints 
data Token = Semicolon | Pipe | Epsilon | Colon | Something
data IR = Undefined2


tokenize :: String -> Token
tokenize ";" = Semicolon
tokenize "|" = Pipe
tokenize "epsilon" = Epsilon
tokenize ":" = Colon
tokenize _ = Something

grammarScan :: String -> ([Token], [Symbol])
grammarScan fileString = (map tokenize (words fileString), words fileString)


--IR, symbol table, list of non-terminals You can move the generation of the symbol table to here if you want.
grammarParse :: ([Token], [Symbol]) -> (IR, [Terminal], [NonTerminal]) 
grammarParse = undefined
