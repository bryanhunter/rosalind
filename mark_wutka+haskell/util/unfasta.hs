import System.IO
import System.Environment
import Text.Printf
import Data.List
import Roselib.FASTA

main = do
  argv <- getArgs
  fileLines <- readFile (head argv)
  putStr . unlines $ map dna (parseFASTA (lines fileLines))
