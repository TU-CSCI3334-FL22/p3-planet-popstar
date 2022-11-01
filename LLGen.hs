module LLGen where
import Reader
import Data.List

type FirstTable = [(Symbol, [Terminal])]
type FollowTable = [(NonTerminal, [Symbol])]
type NextTable = [(Int, [Terminal])]
type Trailer = [Terminal]

initializedFollowTable = [("Goal",["_eof"]),("Goal",[]),("Expr",[]),("EPrime",[]),("Term",[]),("TPrime",[]),("Factor",[])]
sampleFirstTable = [("DIV",["DIV"]),("EPrime",["MINUS","PLUS","_epsilon"]),("Expr",["IDENTIFIER","LP","NUMBER"]),("Factor",["IDENTIFIER","LP","NUMBER"]),("Goal",["IDENTIFIER","LP","NUMBER"]),("IDENTIFIER",["IDENTIFIER"]),("LP",["LP"]),("MINUS",["MINUS"]),("NUMBER",["NUMBER"]),("PLUS",["PLUS"]),("RP",["RP"]),("TIMES",["TIMES"]),("TPrime",["DIV","TIMES","_epsilon"]),("Term",["IDENTIFIER","LP","NUMBER"])]
sampleNonTerminals = ["Goal","Expr","EPrime","Term","TPrime","Factor"]



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

firstOfProductions :: [(Int, Production)] -> FirstTable -> FirstTable
firstOfProductions [] ft = ft
firstOfProductions ((_, p):ps) ft = 
    let newTable = firstOfProduction p ft
    in firstOfProductions ps newTable 

repeatFirst :: [(Int, Production)] -> FirstTable -> FirstTable
repeatFirst prods ft = 
    let newTable = sort $ firstOfProductions prods ft
    in if newTable == ft 
        then newTable 
        else repeatFirst prods newTable

makeTableFirst :: IR -> FirstTable
makeTableFirst ir@(IR productions terminals nonterminals) = 
    repeatFirst productions (initializeFirst ir) 


initializeFollow :: IR -> FollowTable
initializeFollow ir = topLevelNT ir ++ followForeach ir
    where followForeach ir@(IR productions terminals nonterminals) = [(x, []) | x <- tail nonterminals]
          topLevelNT ir@(IR productions terminals nonterminals) = [((head nonterminals), ["_eof"])] 

followProductionHelper :: [Symbol] -> Trailer -> FirstTable -> FollowTable -> [NonTerminal] -> (Trailer, FollowTable) --[Terminal]
followProductionHelper [] trailer firstTable followTable nonterminals = (trailer, followTable)
followProductionHelper (x:xs) trailer firstTable followTable nonterminals = 
    let followTableNew = if x `elem` nonterminals then unionValue x trailer followTable else followTable
        trailerNew = if "_epsilon" `elem` (getValue x firstTable)
                    then nub $ trailer ++ ((getValue x firstTable) \\ ["_epsilon"])
                    else getValue x firstTable
    in followProductionHelper xs trailerNew firstTable followTableNew nonterminals

followOfProduction :: Production -> FirstTable -> FollowTable -> IR -> FollowTable
followOfProduction (lhs, rhs) firstT followT ir@(IR productions terminals nonTerminals) = 
    let reverseRHS = reverse rhs
        (newTrailer, newFollowTable) = followProductionHelper reverseRHS (getValue lhs followT) firstT followT nonTerminals
    in newFollowTable
 
followOfProductions :: [(Int, Production)] -> FirstTable ->  FollowTable -> IR -> FollowTable
followOfProductions [] firstT followT ir = followT 
followOfProductions ((_, p):ps) firstT followT ir = 
    let newFollowTable = followOfProduction p firstT followT ir
    in followOfProductions ps firstT newFollowTable ir

repeatFollow :: FollowTable -> FirstTable -> IR -> FollowTable
repeatFollow followT firstT ir@(IR productions terminals nonterminals) = 
    let newTable = sort $ followOfProductions productions firstT followT ir
    in if newTable == followT 
            then newTable 
            else repeatFollow newTable firstT ir

makeTableFollow :: IR -> FirstTable -> FollowTable 
makeTableFollow ir firstT =
    repeatFollow (initializeFollow ir) firstT ir

makeTableNext :: IR -> FirstTable -> FollowTable -> NextTable
makeTableNext (IR productions terminals nonterminals) firstT followT =
    [makeNextProd p firstT followT | p <- productions]

makeNextProd :: (Int, Production) -> FirstTable -> FollowTable -> (Int, [Terminal])
makeNextProd (x, (lhs, rhs)) firstT followT =
    let result = firstOfRHS rhs firstT
    in if "_epsilon" `elem` result
        then (x, result ++ getValue lhs followT)
        else (x, result)

makeTableWorklist = undefined

showTables ::  (FirstTable, FollowTable, NextTable) -> String
showTables = undefined

toYaml ::  (FirstTable, FollowTable, NextTable) -> Maybe String
toYaml = undefined

fixLL :: (IR, [Terminal], [NonTerminal])  -> (IR, [Terminal], [NonTerminal]) 
fixLL = undefined
