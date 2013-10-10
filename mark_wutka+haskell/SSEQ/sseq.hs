import System.IO
import System.Environment
import Text.Printf
import Data.List
import Roselib.FASTA
import Debug.Trace

subsequenceLocations :: String -> String -> Int -> [Int]
subsequenceLocations [] _ _ = []
subsequenceLocations _ [] _ = []
subsequenceLocations (s:ss) (t:ts) pos =
    if s == t then
        pos : subsequenceLocations ss ts (pos + 1)
    else
        subsequenceLocations ss (t:ts) (pos + 1)

main = do
  argv <- getArgs
  fileLines <- readFile (head argv)
  let [s, t] = map dna . parseFASTA $ lines fileLines
  putStrLn . unwords . map show $ subsequenceLocations s t 1