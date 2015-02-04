import System.IO
import System.Environment
import Text.Printf
import Data.List
import Roselib.Newick
import Debug.Trace

getNodeNames tree =
  let childrenNames = filter (not . null) $ concatMap getNodeNames (children tree) in
  if null (nodeName tree) then
      childrenNames
  else
      (nodeName tree) : childrenNames

splitValue nodeNames node =
    if node `elem` nodeNames then '1' else '0'

printSplit :: [String] -> [String] -> IO ()
printSplit nodeNames nodes = do
    putStrLn $ map (splitValue nodes) nodeNames

printSplits :: [String] -> TreeNodeType -> IO ()
printSplits nodeNames tree = do
  let treeNames = getNodeNames tree
  if length treeNames > 1 && length treeNames < length nodeNames - 1 then
      printSplit nodeNames treeNames
  else
      return ()
  mapM (printSplits nodeNames) (children tree)
  return ()

main = do
  argv <- getArgs
  fileData <- readFile (head argv)
  let Right tree = parseNewick fileData
  let nodeNames = sort $ getNodeNames tree
  printSplits nodeNames tree
  
