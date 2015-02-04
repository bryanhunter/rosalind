import System.IO
import System.Environment
import Text.Printf
import Data.List as List
import Data.Map as Map
import Control.Monad.State

data TrieType = Trie { nodeNumber :: Int, children :: Map Char TrieType } deriving Show

-- Recursively numbers each node in the trie using a state monad to hold the next node number
numberTrie :: (Char,TrieType) -> State Int (Char,TrieType)
numberTrie (ch,(Trie _ trieChildren)) = do
  n <- get
  put (n+1)
  newChildren <- mapM numberTrie (Map.toList trieChildren)
  return (ch,(Trie n (Map.fromList newChildren)))

-- Recursively adds a string to the trie
addToTrie :: TrieType -> String -> TrieType
addToTrie root [] = root
addToTrie (Trie n trieChildren) (s:ss) =
    let newChild = addToTrie (findWithDefault (Trie 0 Map.empty) s trieChildren) ss in
    Trie n (Map.insert s newChild trieChildren)

printTrieAssoc :: TrieType -> (Char,TrieType) -> IO TrieType
printTrieAssoc trie1 (ch,trie2) = do
  putStrLn ((show (nodeNumber trie1)) ++ " " ++ (show (nodeNumber trie2)) ++ " " ++ [ch])
  return trie2
  

-- Print the adjacency list for this node and its children, then do the same for each child
walkTrie :: TrieType -> IO ()
walkTrie trie = do
  mapM (printTrieAssoc trie) (Map.toList (children trie))
  result <- mapM walkTrie (List.map snd (Map.toList (children trie)))
  return ()
  
main = do
  argv <- getArgs
  fileData <- readFile (head argv)
  let strs = lines fileData
  let root = Trie 0 Map.empty
  let newRoot = List.foldl' addToTrie root strs
  let ((_,finalTrie),_) = runState (numberTrie (' ',newRoot)) 1
  walkTrie finalTrie

  

