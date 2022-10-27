module LLGen where
import Reader
import Data.List

type FirstTable = [(Symbol, [Terminal])]
type FollowTable = [(NonTerminal, [Symbol])]
type NextTable = [(Int, [Symbol])]

makeTableFirstHelper :: IR -> NonTerminal -> [Terminal] 
makeTableFirstHelper ir@(IR productions terminals _) nonTerminal = 
    let associatedSymbols = [ s | (x, Symbol s:xs) <- productions, x == nonTerminal]
        (tts, ntts) = partition (`elem` terminals) associatedSymbols
    in tts ++ concat [makeTableFirstHelper ir t | t <- ntts]

makeTableFirst :: (IR, [Token]) -> FirstTable
makeTableFirst (ir@(IR _ terminals nonTerminals), tokens) = 
    let terminalList = [(x, [x]) | x <- terminals]
        finalNTList = [(x, makeTableFirstHelper ir x) | x <- nonTerminals]
    in finalNTList++terminalList 

makeTableFollow = undefined 
makeTableNext = undefined 
makeTableWorklist = undefined

-- makeTables :: (IR, [Terminal], [NonTerminal]) -> Bool -> (FirstTable, FollowTable, NextTable)
-- makeTables :: (IR, [Terminal], [NonTerminal]) -> FirstTable
-- makeTables ((IR productions, terminals, nonterminals), givenTerminals, )= undefined

--- ignore for now ^^ 

showTables ::  (FirstTable, FollowTable, NextTable) -> String
showTables = undefined

toYaml ::  (FirstTable, FollowTable, NextTable) -> Maybe String
toYaml = undefined

fixLL :: (IR, [Terminal], [NonTerminal])  -> (IR, [Terminal], [NonTerminal]) 
fixLL = undefined
