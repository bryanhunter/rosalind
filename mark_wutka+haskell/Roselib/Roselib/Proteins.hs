module Roselib.Proteins
(getProteinMass, getProteinByMass)
where

import Data.List as List
import qualified Data.Map as Map

proteinMassTable = Map.fromList [
   ('A',71.03711),
    ('C',103.00919),
    ('D',115.02694),
    ('E',129.04259),
    ('F',147.06841),
    ('G',57.02146),
    ('H',137.05891),
    ('I',113.08406),
    ('K',128.09496),
    ('L',113.08406),
    ('M',131.04049),
    ('N',114.04293),
    ('P',97.05276),
    ('Q',128.05858),
    ('R',156.10111),
    ('S',87.03203),
    ('T',101.04768),
    ('V',99.06841),
    ('W',186.07931),
    ('Y',163.06333) ]

getProteinMass prot =
    Map.findWithDefault 0.0 prot proteinMassTable

massMatch m1 m2 =
    abs (m1 - m2) < 0.001

getProteinByMass m =
    head $ List.filter (\x-> massMatch m (snd x)) (Map.toList proteinMassTable)
