import System.IO
import System.Environment
import Text.Printf
import Data.List

-- This is a bit of a trick. If there are N nodes in the tree, there are N-1
-- edges. Thus, the number of edges required to make the given set of nodes
-- into a tree is just N - # of edges - 1.
-- For this particular problem, you don't actually have to reconstruct the tree

readInt :: String -> Int
readInt = read

main = do
  argv <- getArgs
  filedata <- readFile (head argv)
  let fileLines = lines filedata
  let numNodes = readInt $ head fileLines
  putStrLn . show $ numNodes - (length $ tail fileLines) - 1
