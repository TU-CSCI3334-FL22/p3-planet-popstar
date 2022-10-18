module LLGen where
import Reader

type FirstTable = [(Symbol, [Symbol])]
type FollowTable = [(NonTerminal, [Symbol])]
type NextTable = [(Int, [Symbol])]

makeTables :: (IR, [Terminal], [NonTerminal]) -> Bool -> (FirstTable, FollowTable, NextTable)
makeTables = undefined

showTables ::  (FirstTable, FollowTable, NextTable) -> String
showTables = undefined

toYaml ::  (FirstTable, FollowTable, NextTable) -> Maybe String
toYaml = undefined

fixLL :: (IR, [Terminal], [NonTerminal])  -> (IR, [Terminal], [NonTerminal]) 
fixLL = undefined
