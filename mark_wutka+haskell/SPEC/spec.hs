import System.IO
import System.Environment
import Text.Printf
import Data.List
import Roselib.Proteins

readDouble :: String -> Double
readDouble s = read s

main = do
  argv <- getArgs
  fileData <- readFile (head argv)
  let weights = map readDouble (lines fileData)
  let weightDiffs = zipWith (-) (tail weights) weights
  putStrLn $ map (fst . getProteinByMass) weightDiffs
  
