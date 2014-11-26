module Roselib.Utils
( hammingDistance, charCounts)
where

import Data.List
import qualified Data.Map as Map

hammingDistance :: String -> String -> Int
hammingDistance s1 s2 =
    length $ filter id $ zipWith (/=) s1 s2

charCounts :: String -> Map.Map Char Int
charCounts s =
    Map.fromListWith (+) [(c,1) | c <- s]
