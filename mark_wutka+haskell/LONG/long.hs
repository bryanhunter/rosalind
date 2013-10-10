import System.IO
import System.Environment
import Text.Printf
import Data.List
import Roselib.FASTA
import Debug.Trace


findEnd strands =
   head $ filter (noFollowers strands) strands

noFollowers strands strand =
   null $ candidateStrands strand (delete strand strands)

findPrefix chain [] = chain
findPrefix chain strands =
        findPrefix (candidatePrefix : chain) (delete candidatePrefix strands)
   where
        candidatePrefix = head $ candidatePrefixes (head chain) strands

candidatePrefixes suffix strands = filter (\p -> overlaps p suffix) strands

candidateStrands prefix strands = filter (overlaps prefix) strands

overlayLengths x y =  [(min (length x) (length y))`div` 2 .. (min (length x) (length y))-1]

overlaps x y = any id $ map (tryOverlay x y) (overlayLengths x y)

tryOverlay x y n = (reverse $ take n (reverse x)) == (take n y)

glue chain = foldl1' mergeStrands chain
mergeStrands x y = x ++ (drop (mergeLen x y) y)
mergeLen x y = head $ filter (tryOverlay x y) (overlayLengths x y)

mergeSequences strands = 
    glue $ findPrefix [lastItem] (delete lastItem strands)
         where lastItem = findEnd strands

main = do
  argv <- getArgs
  fileLines <- readFile (head argv)
  let strands = map dna $ parseFASTA (lines fileLines)
  putStrLn $ mergeSequences strands