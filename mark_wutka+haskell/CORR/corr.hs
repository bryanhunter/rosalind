import System.IO
import System.Environment
import Text.Printf
import Data.List as List
import Data.Map as Map
import Data.Maybe
import Roselib.FASTA
import Roselib.DNA
import Roselib.Utils

buildTable :: [String] -> Map String Int
buildTable dnas =
    Map.fromListWith (+) [ (s,1) | s <- dnas ]

needsCorrection :: Map String Int -> String -> Bool
needsCorrection table s =
    Map.findWithDefault 0 s table < 2

findCorrection :: Map String Int -> String -> Maybe (String,Int)
findCorrection table s =
    find correctionMatch (Map.toList table)
        where
          correctionMatch (t,tcount) = (tcount > 1) && (hammingDistance s t == 1)

addCorrection :: [String] -> String -> Maybe (String,Int) -> [String]
addCorrection corrs _ Nothing = corrs
addCorrection corrs s (Just (t,tcount)) = (s ++ "->" ++ t) : corrs

getCorrection :: Map String Int -> [String] -> String -> [String]
getCorrection table corrs s =
    addCorrection corrs s (findCorrection table s)
                 
getCorrections :: [String] -> Map String Int -> [String]
getCorrections dnas table =
    reverse $ List.foldl (getCorrection table) [] (List.filter (needsCorrection table) dnas)

main = do
  argv <- getArgs
  fileData <- readFile (head argv)
  let fasta = parseFASTA $ lines fileData
  let fasta_DNA = List.map dna fasta
  let table = buildTable (fasta_DNA ++ (List.map reverseComplement fasta_DNA))
  putStrLn (unlines (getCorrections fasta_DNA table))
