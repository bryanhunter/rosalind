module Roselib.Newick
(TreeNodeType(..), parseNewick, parseNewickWithNodeNumbering)
where

import Text.ParserCombinators.Parsec
import Data.Maybe

data TreeNodeType = TreeNode { nodeName :: String, children :: [TreeNodeType] } deriving Show

tree = do
  node <- subtree
  char ';'
  return node

subtree = try internal <|> leaf

leaf = do
  n <- name
  return (TreeNode n [])

name :: CharParser (Maybe Int) String
name = do
  n <- many (noneOf ";,)")
  num <- getState
  if ((length n) == 0) && (isJust num) then
      do {
        setState (Just (1 + (fromJust num)));
                 return (";"++(show (fromJust num))) }
  else
      return n

internal = do
  char '('
  children <- branchSet
  char ')'
  n <- name
  return (TreeNode n children)

branchSet = sepBy subtree (char ',')

parseNewick = runParser tree Nothing "(input)"

parseNewickWithNodeNumbering = 
    runParser tree (Just 0) "(input)"
