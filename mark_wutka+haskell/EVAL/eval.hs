import System.IO
import System.Environment
import Text.Printf
import Data.List
import qualified Data.Map as Map
import Roselib.Probability

expectedSubstrings :: Int -> String -> Double -> Double
expectedSubstrings n dna a =
    combinedProbs * fromIntegral (n - (length dna) + 1)
        where
          probs = gcProbabilityTable a
          combinedProbs = foldl combineProb 1.0 dna
          combineProb a c = a * (probs Map.! c)

allExpectedSubstrings :: Int -> String -> [Double] -> [Double]
allExpectedSubstrings n dna a =
    map (expectedSubstrings n dna) a
                   
main = do
  argv <- getArgs
  fileData <- readFile (head argv)
  let (nstr:dna:astr:_) = lines fileData
  let n = read nstr
  let a = map read (words astr)
  putStrLn $ unwords $ map show (allExpectedSubstrings n dna a) 
