import System.IO
import System.Environment
import Text.Printf
import Data.List
import Roselib.FASTA
import Debug.Trace

-- This solution builds the chain by first finding the end and then
-- finding the prefix of the end strand, recursively back until there
-- are no more strands.

-- Finds the last strand in the chain, the one that has no followers
findEnd :: [String] -> String
findEnd strands =
   head $ filter (noFollowers strands) strands

-- Looks for a strand whose set of candidate suffixes is empty
noFollowers :: [String] -> String -> Bool
noFollowers strands strand =
   null $ candidateSuffixes strand (delete strand strands)

-- Finds the strand that comes before the first strand in the chain
findPrefix :: [String] -> [String] -> [String]
findPrefix chain [] = chain
findPrefix chain strands =
        findPrefix (candidatePrefix : chain) (delete candidatePrefix strands)
   where
        candidatePrefix = head $ candidatePrefixes (head chain) strands

-- Finds all the strands that can come before a particular suffix
candidatePrefixes :: String -> [String] -> [String]
candidatePrefixes suffix strands = filter (\p -> overlaps p suffix) strands

-- Finds all the strands that can come after a particular prefix
candidateSuffixes :: String -> [String] -> [String]
candidateSuffixes prefix strands = filter (overlaps prefix) strands


-- Returns true if x and y overlap by at least half their length
overlaps :: String -> String -> Bool
overlaps x y = or $ map (tryOverlay x y) (overlapLengths x y)

-- Returns a list of all the possibly lengths of overlaps
overlapLengths :: String -> String -> [Int]
overlapLengths x y =  [(min (length x) (length y))`div` 2 .. (min (length x) (length y))-1]

-- Returns true if try strings overlap by n chars
tryOverlay :: String -> String -> Int -> Bool
tryOverlay x y n = (reverse $ take n (reverse x)) == (take n y)

-- Glues the chain of strands together
glue :: [String] -> String
glue chain = foldl1' mergeStrands chain
     where
        mergeStrands x y = x ++ (drop (mergeLen x y) y)
        mergeLen x y = head $ filter (tryOverlay x y) (overlapLengths x y)

-- Marges the sequences together by finding the end and then finding each prefix to the
-- previous item in the chain
mergeSequences :: [String] -> String
mergeSequences strands = 
    glue $ findPrefix [lastItem] (delete lastItem strands)
         where lastItem = findEnd strands

main = do
  argv <- getArgs
  fileLines <- readFile (head argv)
  let strands = map dna $ parseFASTA (lines fileLines)
  putStrLn $ mergeSequences strands