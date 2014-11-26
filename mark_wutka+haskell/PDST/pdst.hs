import System.IO
import System.Environment
import Text.Printf
import Data.List
import Roselib.FASTA
import Roselib.Utils

computePDistance :: [String] -> [[Double]]
computePDistance dnas =
    [ computePDistanceForDNA d | d <- dnas ]
        where
          computePDistanceForDNA dna =
              [ (fromIntegral $ hammingDistance dna d) / (fromIntegral $ length dna) | d <- dnas ]
main = do
  argv <- getArgs
  fileData <- readFile (head argv)
  let fasta = parseFASTA $ lines fileData
  let fasta_DNA = map dna fasta
  putStrLn $ unlines $ map (unwords . map show) (computePDistance fasta_DNA) 
