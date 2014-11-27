module Roselib.Probability
( randomProbability, gcProbability, gcProbabilityTable)
where

import Data.List as List
import Data.Map as Map

randomProbability :: String -> Map Char Double -> Double
randomProbability s probs =
    List.foldl (*) 1.0 $ List.map charProb s
        where
          charProb c = findWithDefault 0.0 c probs

gcProbabilityTable :: Double -> Map Char Double
gcProbabilityTable gcContent =
    Map.fromList [ ('A',atProb), ('C', gcProb), ('G', gcProb), ('T',atProb) ]
        where
          gcProb = gcContent / 2.0
          atProb = (1.0 - gcContent) / 2.0

gcProbability :: String -> Double -> Double
gcProbability s gcContent =
    randomProbability s $ gcProbabilityTable gcContent
