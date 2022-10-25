module LLGen where
import Reader

type FirstTable = [(Symbol, [NonTerminal])]
type FollowTable = [(NonTerminal, [Symbol])]
type NextTable = [(Int, [Symbol])]


makeTableFirst :: (IR, [Token]) -> FirstTable
makeTableFirst = 



makeTableFollow = undefined 
makeTableNext = undefined 
makeTableWorklist = undefined

makeTables :: (IR, [Terminal], [NonTerminal]) -> Bool -> (FirstTable, FollowTable, NextTable)
makeTables = undefined

--- ignore for now ^^ 

showTables ::  (FirstTable, FollowTable, NextTable) -> String
showTables = undefined

toYaml ::  (FirstTable, FollowTable, NextTable) -> Maybe String
toYaml = undefined

fixLL :: (IR, [Terminal], [NonTerminal])  -> (IR, [Terminal], [NonTerminal]) 
fixLL = undefined
