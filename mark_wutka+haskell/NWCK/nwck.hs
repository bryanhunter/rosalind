import System.IO
import System.Environment
import Text.Printf
import Roselib.Newick
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace

getJust [] = Nothing
getJust (x:xs) =
    if isJust x then
        x
    else
        getJust xs

combine s Nothing = Nothing
combine s (Just xs) = Just (s:xs)

pathToNode :: String -> TreeNodeType -> Maybe [String]
pathToNode node (TreeNode treeNode treeChildren) =
    if node == treeNode then
        Just [node]
    else if (length treeChildren) == 0 then
             Nothing
         else
            combine treeNode (getJust (map (pathToNode node) treeChildren))

computeDistance x [] = length x
computeDistance [] y = length y
computeDistance (p1:p1s) (p2:p2s) =
    if p1 == p2 then
        computeDistance p1s p2s
    else
        (length (p1:p1s)) + (length (p2:p2s))

getDistance [treeStr, nodes] =
    let Right tree = parseNewickWithNodeNumbering treeStr in
    let [node1, node2] = words nodes in
    let Just node1Path = pathToNode node1 tree in
    let Just node2Path = pathToNode node2 tree in
    computeDistance node1Path node2Path

main = do
  argv <- getArgs
  fileData <- readFile (head argv)
  let fileLines = lines fileData
  let newickSets = map (take 2) $ chunksOf 3 fileLines
  putStrLn (unwords (map (show . getDistance) newickSets))
