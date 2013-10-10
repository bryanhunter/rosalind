import System.IO
import System.Environment
import Text.Printf
import Data.List
import qualified Data.Map as Map

readFloat :: String -> Double
readFloat = read

randomProbability [] probs prob = prob
randomProbability (d:ds) probs prob =
    randomProbability ds probs (prob * (probs Map.! d))
   
computeRandomProbability :: String -> Double -> Double
computeRandomProbability s gcContent =
    randomProbability s probTable 1.0
    where
        probTable = Map.fromList [ ('A', atProb), ('C', gcProb),
                                   ('G', gcProb), ('T', atProb) ]
        atProb = (1.0 - gcContent) / 2.0
        gcProb = gcContent / 2.0
   
main = do
  argv <- getArgs
  fileContents <- readFile (head argv)
  let fileLines = lines fileContents
  let dna = head fileLines
  let a = map ((logBase 10.0) . computeRandomProbability dna . readFloat) (words . head $ tail fileLines)
  putStrLn . unwords $ map show a
