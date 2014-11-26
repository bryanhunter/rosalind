import System.IO
import System.Environment
import Text.Printf
import Data.List
import Data.Map as Map
import Roselib.FASTA
import Roselib.Utils
import Roselib.Combinations

maximumMatches :: String -> Integer
maximumMatches s =
    auCombos * cgCombos
             where
               counts = charCounts s
               auCombos = comboCounts $ maxCounts counts 'A' 'U'
               cgCombos = comboCounts $ maxCounts counts 'C' 'G'
               comboCounts (n1,n2) = (factorial (toInteger n1)) `div` factorial (toInteger (n1 - n2))
               maxCounts counts c1 c2 = if counts ! c1 > counts ! c2 then
                                            (counts ! c1, counts ! c2)
                                        else
                                            (counts ! c2, counts ! c1)
main = do
  argv <- getArgs
  fileData <- readFile (head argv)
  let fasta = parseFASTA $ lines fileData
  let fasta_DNA = Data.List.map dna fasta
  let s = head fasta_DNA
  putStrLn . show $ maximumMatches s
