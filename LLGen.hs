module LLGen where
import Reader

type FirstTable = [(Symbol, [Terminal])]
type FollowTable = [(NonTerminal, [Symbol])]
type NextTable = [(Int, [Symbol])]

makeTableFirstHelper :: (IR, [Token]) -> [Terminal] 
makeTableFirstHelper ((IR productions, terminals, nonterminals), tokens) = 
    undefined 

makeTableFirst :: (IR, [Token]) -> FirstTable
makeTableFirst ((IR productions, terminals, nonterminals), tokens) = 
    let terminalList = [ (x, [x]) | x <- terminals ] -- purely terminals
        finalNTList = [ (y, makeTableFirstHelper y) | y <- nonterminals ]
    in finalNTList++terminalList 

makeTableFollow = undefined 
makeTableNext = undefined 
makeTableWorklist = undefined

-- makeTables :: (IR, [Terminal], [NonTerminal]) -> Bool -> (FirstTable, FollowTable, NextTable)
makeTables :: (IR, [Terminal], [NonTerminal]) -> FirstTable
makeTables ((IR productions, terminals, nonterminals), givenTerminals, )= undefined

--- ignore for now ^^ 

showTables ::  (FirstTable, FollowTable, NextTable) -> String
showTables = undefined

toYaml ::  (FirstTable, FollowTable, NextTable) -> Maybe String
toYaml = undefined

fixLL :: (IR, [Terminal], [NonTerminal])  -> (IR, [Terminal], [NonTerminal]) 
fixLL = undefined
