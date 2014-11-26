import System.IO
import System.Environment
import Text.Printf
import Data.List
import Roselib.Probability

computeProbability :: Int -> Double -> String -> Double
computeProbability n x s =
    foldl accProb prob [1..n]
        where
          prob = gcProbability s x
          accProb a n = a + prob * (1.0 - a)

main = do
  argv <- getArgs
  fileData <- readFile (head argv)
  let (probLine:dna:_) = lines fileData
  let (nstr:xstr:_) = words probLine
  let n = read nstr
  let x = read xstr
  putStrLn . show $ computeProbability n x dna
                        
