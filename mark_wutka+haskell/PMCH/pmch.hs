import System.IO
import System.Environment
import Text.Printf
import Data.Map
import Roselib.Combinations
import Roselib.FASTA

perfectMatchings rna =
  (factorial a_count) * (factorial c_count)
  where
        a_count = counts ! 'A'
        c_count = counts ! 'C'
        counts = fromListWith (+) [(c,1) | c <- rna]
 
main = do
  argv <- getArgs
  fileLines <- readFile (head argv)
  let rna = dna . head . parseFASTA $ lines fileLines
  putStrLn . show $ perfectMatchings rna