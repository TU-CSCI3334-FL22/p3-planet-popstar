module LLGen where
import Reader
import Data.List

type FirstTable = [(Symbol, [Terminal])]
type FollowTable = [(NonTerminal, [Symbol])]
type NextTable = [(Int, [Symbol])]

initializeFirst :: IR -> FirstTable 
initializeFirst ir  = firstForeach ir ++ secondForeach ir
    where firstForeach ir@(IR productions terminals nonterminals) = [(x, [x]) | x <- terminals]
          secondForeach ir@(IR productions terminals nonterminals) = [(x, []) | x <- nonterminals]

getValue :: Eq a => a -> [(a, [b])] -> [b] 
getValue key table = 
    case [ v | (k,v) <- table, k == key] of 
        [v] -> v 
        [] -> []
        _ -> error "too many things in table "

unionValue :: (Eq a, Ord a, Ord b) => a -> [b] -> [(a, [b])] -> [(a, [b])] 
unionValue key newVals table = 
    let currentVals = getValue key table
        appendedVals = sort $ nub $ currentVals ++ newVals 
    in (key, appendedVals):[ (k, v)| (k, v) <- table, k /= key]


firstOfRHS :: [Symbol] ->  FirstTable -> [Terminal]
firstOfRHS [] ft = ["_epsilon"]
firstOfRHS (x:xs) ft = 
    let fx = getValue x ft
    in if "_epsilon" `elem` fx 
        then (firstOfRHS xs ft) ++ (fx \\ ["_epsilon"])
        else fx  

firstOfProduction :: Production -> FirstTable -> FirstTable
firstOfProduction(lhs, rhs) ft = 
    let rhsVal = firstOfRHS rhs ft
    in unionValue lhs rhsVal ft 

firstOfProductions :: [Production] -> FirstTable -> FirstTable
-- firstOfProductions ps ft = foldr firstOfProduction ft ps
firstOfProductions [] ft = ft
firstOfProductions (p:ps) ft = 
    let newTable = firstOfProduction p ft
    in firstOfProductions ps newTable 

repeatFirst :: [Production] -> FirstTable -> FirstTable
repeatFirst prods ft = 
    let newTable = sort $ firstOfProductions prods ft
    in if newTable == ft 
        then newTable 
        else repeatFirst prods newTable

makeTableFirst :: IR -> FirstTable
makeTableFirst ir@(IR productions terminals nonterminals) = 
    repeatFirst productions (initializeFirst ir) 

makeTableFollow = undefined 
makeTableNext = undefined 
makeTableWorklist = undefined

showTables ::  (FirstTable, FollowTable, NextTable) -> String
showTables = undefined

toYaml ::  (FirstTable, FollowTable, NextTable) -> Maybe String
toYaml = undefined

fixLL :: (IR, [Terminal], [NonTerminal])  -> (IR, [Terminal], [NonTerminal]) 
fixLL = undefined
