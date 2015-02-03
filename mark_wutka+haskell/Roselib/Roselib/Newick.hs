module Roselib.Newick
(TreeNodeType(..), parseNewick)
where

import Text.ParserCombinators.Parsec

data TreeNodeType = TreeNode { nodeName :: String, children :: [TreeNodeType] } deriving Show

tree = do
  node <- subtree
  char ';'
  return node

subtree = try internal <|> leaf

leaf = do
  n <- name
  return (TreeNode n [])
name = many (noneOf ";,)")

internal = do
  char '('
  children <- branchSet
  char ')'
  n <- name
  return (TreeNode n children)

branchSet = sepBy subtree (char ',')

parseNewick = parse tree "(input)"
