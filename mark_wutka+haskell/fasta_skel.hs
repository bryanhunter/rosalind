import System.IO
import System.Environment
import Text.Printf
import Data.List
import Roselib.FASTA

main = do
  argv <- getArgs
  fileData <- readFile (head argv)
  let fasta = parseFASTA $ lines fileData
  let fasta_DNA = map dna fasta
