import System.IO
import System.Environment
import Text.Printf
import Data.List as List
import Data.Set as Set
import Roselib.DNA

makePair kmer =
    (reverse . tail . reverse $ kmer, tail kmer)

printPair (a,b) = do
  putStrLn $ "("++a++", "++b++")"

printPairs kmerSet = do
    mapM printPair (Set.toList kmerSet)

main = do
  argv <- getArgs
  fileData <- readFile (head argv)
  let kmers = lines fileData
  let kmerSet = Set.fromList $ List.map makePair (kmers ++ (List.map reverseComplement kmers))
  printPairs kmerSet
  
