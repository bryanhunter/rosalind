import System.IO
import System.Environment
import Text.Printf
import Data.List as List
import Data.Map as Map
import Roselib.FASTA

allKeys :: [String]
allKeys = [ [c1, c2, c3, c4 ] | c1 <- ['A','C','G','T'], c2 <- ['A','C','G','T'],
                                c3 <- ['A','C','G','T'], c4 <- ['A','C','G','T']]
                                                                   
getKMerCounts :: String -> Map String Int
getKMerCounts s =
    Map.fromListWith (+) ([ (sinit,0) | sinit <- allKeys ] ++
           [ (s, 1) | s <- List.filter (\x -> length x == 4) (List.map (take 4) (tails s)) ])

main = do
  argv <- getArgs
  fileData <- readFile (head argv)
  let fasta = parseFASTA $ lines fileData
  let s = dna (head fasta)
  putStrLn . unwords . List.map show $ Map.elems $ getKMerCounts s
