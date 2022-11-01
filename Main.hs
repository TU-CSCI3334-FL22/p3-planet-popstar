module Main where
import Data.Char
import Debug.Trace
import Data.List
import System.Console.GetOpt
import System.Environment
import Control.Monad
import Data.Maybe
import System.Exit
import Reader
import LLGen

data Options = Options {
   optHelp :: Bool
 , optTable :: Bool
--  , optRevise :: Bool
--  , optWorklist :: Bool
 , fname :: String
 }

defaultOptions :: Options
defaultOptions = Options {
      optHelp = False
    , optTable  = False
    -- , optRevise = False
    -- , optWorklist = False
    , fname = ""
 }

options :: [OptDescr (Options -> Options)]
options = [
  Option ['h'] ["help"]   (NoArg  (\opts -> opts { optHelp = True })) "Print a help message and exit.",
  Option ['t'] ["table"]   (NoArg  (\opts -> opts { optTable = True })) "Print YAML tables instead of human-readable output."
  -- Option ['r'] ["revise"]   (NoArg  (\opts -> opts { optRevise= True })) "Attempt to revise the grammar if it is not LL(1).",
  -- Option ['w'] ["worklist"]   (NoArg  (\opts -> opts { optWorklist= True })) "Use the worklist version of First and Follow."
  ]

compilerOpts :: [String] -> Options
compilerOpts argv =
  case getOpt Permute options argv of
     (o,[x],[]) -> foldl (flip id) (defaultOptions {fname = x}) o
     (o,[],[]) -> foldl (flip id) defaultOptions o
     (_,_,[]) -> error (usageInfo header options)
     (_,_,errs) -> error (concat errs ++ usageInfo header options)
  where header = "Usage: ./llgen [OPTION]... [file]"

helpIO :: IO()
helpIO = putStrLn $ usageInfo usage options
  where usage = "Usage: ./llgen [OPTION]... [file]"

makeTables :: IR -> (FirstTable, FollowTable, NextTable, PredictionTable)
makeTables ir = 
  let firstT = makeTableFirst ir
      followT = makeTableFollow ir firstT
      nextT = makeTableNext ir firstT followT
      predictionT = makePredictionTable ir nextT
    in (firstT, followT, nextT, predictionT)


showTables :: IR -> (FirstTable, FollowTable, NextTable, PredictionTable) -> IO()
showTables (IR productions _ _) (firstT, followT, nextT, predictionT) = do
  putStrLn "Productions:"
  sequence $ map print productions
  putStrLn "\nFirst Table:"
  sequence $ map print firstT
  putStrLn "\nFollow Table:"
  sequence $ map print followT
  putStrLn "\nNext Table:"
  sequence $ map print nextT
  putStrLn "\nPrediction Table:"
  sequence $ map print predictionT
  return ()

toYaml :: IR -> (FirstTable, FollowTable, NextTable, PredictionTable) -> IO()
toYaml = undefined

main :: IO() 
main = do
  allArgs <- getArgs
  let opts = compilerOpts allArgs
  if optHelp opts || fname opts == "" then helpIO
  else do 
        contents <- readFile (fname opts)
        let tokens = grammarScan contents
            ir = parseGrammar tokens
            -- improvedIR = if optRevise opts then fixLL ir else ir
            tables = makeTables ir
        if not $ optTable opts
          then showTables ir tables
          else toYaml ir tables
