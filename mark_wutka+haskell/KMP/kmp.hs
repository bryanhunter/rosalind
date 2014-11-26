import System.IO
import System.Environment
import Text.Printf
import Data.List
import Roselib.FASTA

offsetList [] _ _ = []
offsetList _ [] _ = []
offsetList (s:ss) (p:ps) matchLen =
    if matchLen == 0 then
        []
    else if s /= p then
             []
         else
             (matchLen+1) : offsetList ss ps (matchLen+1)

findMax :: [Int] -> [Int] -> [Int]
findMax [] s = s
findMax s [] = s
findMax (s:ss) (t:tt) =
    (max s t) : findMax ss tt

computePrefixes :: String -> String -> [Int]
computePrefixes s [] = []
computePrefixes (s:ss) (p:ps) =
    if p == s then
        findMax (1 : offsetList ss ps 1) (0 : computePrefixes (s:ss) ps)
    else
        0 : (computePrefixes (s:ss) ps)

main = do
  argv <- getArgs
  fileData <- readFile (head argv)
  let fasta = parseFASTA $ lines fileData
  let fasta_DNA = map dna fasta
  let s = head fasta_DNA
  putStrLn (unwords (map show (0 : computePrefixes s (tail s))))
