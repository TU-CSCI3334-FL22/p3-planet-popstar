module LLGen where
import Reader
import Data.List

type FirstTable = [(Symbol, [Terminal])]
type FollowTable = [(NonTerminal, [Symbol])]
type NextTable = [(Int, [Symbol])]
type Trailer = [Terminal]

--Here follow some test data
sampleIR = IR [("Goal",["Expr"]),("Expr",["Term","EPrime"]),("EPrime",["PLUS","Term","EPrime"]),("EPrime",["MINUS","Term","EPrime"]),("EPrime",[]),("Term",["Factor","TPrime"]),("TPrime",["TIMES","Factor","TPrime"]),("TPrime",["DIV","Factor","TPrime"]),("TPrime",[]),("Factor",["LP","Expr","RP"]),("Factor",["NUMBER"]),("Factor",["IDENTIFIER"])] ["PLUS","MINUS","TIMES","DIV","LP","RP","NUMBER","IDENTIFIER"] ["Goal","Expr","EPrime","Term","TPrime","Factor"]
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

--line 7 + 8
firstOfRHS :: [Symbol] ->  FirstTable -> [Terminal]
firstOfRHS [] ft = ["_epsilon"]
firstOfRHS (x:xs) ft = 
    let fx = getValue x ft
    in if "_epsilon" `elem` fx 
        then (firstOfRHS xs ft) ++ (fx \\ ["_epsilon"])
        else fx  

-- line 11 
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



-- make table follow starts here !! 
-- lines 1 - 3
initializeFollow :: IR -> FollowTable
initializeFollow ir = topLevelNT ir ++ followForeach ir
    where followForeach ir@(IR productions terminals nonterminals) = [(x, []) | x <- tail nonterminals]
          topLevelNT ir@(IR productions terminals nonterminals) = [((head nonterminals), ["_eof"])] 

-- line 9 terribleHelper
loPHelper :: Trailer -> Production -> FollowTable -> FollowTable
loPHelper trailer (lhs, rhs) ft = unionValue lhs trailer ft


-- we will pass in the first table to this function
followProductionHelper :: [Symbol] -> Trailer -> FirstTable -> FollowTable -> [NonTerminal] -> (Trailer, FollowTable) --[Terminal]
followProductionHelper [] trailer firstTable followTable nonterminals = (trailer, followTable)
followProductionHelper (x:xs) trailer firstTable followTable nonterminals = 
    let followTableNew = if x `elem` nonterminals then unionValue x trailer followTable else followTable
        trailerNew = if "_epsilon" `elem` (getValue x firstTable)
                    then nub $ trailer ++ ((getValue x firstTable) \\ ["_epsilon"])
                    else getValue x firstTable
    in followProductionHelper xs trailerNew firstTable followTableNew nonterminals

-- lines 7 - 13
followOfProduction :: Production -> Trailer -> FollowTable -> IR -> FollowTable --[Terminal]
followOfProduction (lhs, rhs) trailer ft ir@(IR productions terminals nonTerminals) = 
    let reverseRHS = reverse rhs
        firstT = makeTableFirst ir
        (newTrailer, newFollowTable) = followProductionHelper reverseRHS trailer firstT ft nonTerminals
    in newFollowTable
 

-- lines 
followOfProductions :: [Production] -> FollowTable -> IR -> FollowTable
followOfProductions [] ft ir = ft 
followOfProductions (p:ps) ft ir = 
    let newFollowTable = followOfProduction p [] ft ir
    in followOfProductions ps newFollowTable ir

-- lines 4
repeatFollow :: [Production] -> FollowTable -> IR -> FollowTable
repeatFollow prods ft ir = 
    let newTable = sort $ followOfProductions prods ft ir
    in if newTable == ft 
            then newTable 
            else repeatFollow prods newTable ir

-- overall table
makeTableFollow :: IR -> FollowTable 
makeTableFollow ir@(IR productions terminals nonterminals) =
    repeatFollow productions (initializeFollow ir) ir

makeTableNext = undefined 
makeTableWorklist = undefined

showTables ::  (FirstTable, FollowTable, NextTable) -> String
showTables = undefined

toYaml ::  (FirstTable, FollowTable, NextTable) -> Maybe String
toYaml = undefined

fixLL :: (IR, [Terminal], [NonTerminal])  -> (IR, [Terminal], [NonTerminal]) 
fixLL = undefined
