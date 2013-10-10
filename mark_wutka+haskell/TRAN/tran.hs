import System.IO
import System.Environment
import Text.Printf
import Data.List
import Roselib.FASTA

transitionInd s1 s2 =
  if ((s1=='A') && (s2=='G')) ||
     ((s1=='G') && (s2=='A')) ||
     ((s1=='C') && (s2=='T')) ||
     ((s1=='T') && (s2=='C')) then 1 else 0

transversionInd s1 s2 =
  if ((s1=='A') && ((s2=='C') || (s2=='T'))) ||
     ((s1=='C') && ((s2=='A') || (s2=='G'))) ||
     ((s1=='G') && ((s2=='C') || (s2=='T'))) ||
     ((s1=='T') && ((s2=='A') || (s2=='G'))) then 1 else 0

main = do
  argv <- getArgs
  fileLines <- readFile (head argv)
  let [s1,s2] = map dna . parseFASTA $ lines fileLines
  let transitionCount = sum $ zipWith transitionInd s1 s2
  let transversionCount = sum $ zipWith transversionInd s1 s2
  putStrLn . show $ (fromIntegral transitionCount) / (fromIntegral transversionCount)
