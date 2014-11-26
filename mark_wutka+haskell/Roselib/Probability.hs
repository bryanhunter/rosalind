module Roselib.Probability
( randomProbability, gcProbability)
where

import Data.List as List
import Data.Map as Map

randomProbability :: String -> Map Char Double -> Double
randomProbability s probs =
    List.foldl (*) 1.0 $ List.map charProb s
        where
          charProb c = findWithDefault 0.0 c probs

gcProbability :: String -> Double -> Double
gcProbability s gcContent =
    randomProbability s gcTable
        where
          gcProb = gcContent / 2.0
          atProb = (1.0 - gcContent) / 2.0
          gcTable = Map.fromList [ ('A',atProb), ('C', gcProb), ('G', gcProb), ('T',atProb) ]
