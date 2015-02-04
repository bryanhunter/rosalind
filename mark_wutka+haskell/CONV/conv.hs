import System.IO
import System.Environment
import Text.Printf
import Data.List

readDouble :: String -> Double
readDouble = read

doubleEqual :: Double -> Double -> Bool
doubleEqual x y = abs (x-y) < 0.0001

lengthCompare x y = compare (length x) (length y)

main = do
  argv <- getArgs
  fileData <- readFile (head argv)
  let (s1str:s2str:_) = lines fileData
  let s1 = map readDouble (words s1str)
  let s2 = map readDouble (words s2str)
  let multi = groupBy doubleEqual $ sort [ a-b | a <- s1, b <- s2]
  let best = head . reverse $ sortBy lengthCompare multi
  putStrLn (show (length best))
  putStrLn (show (abs (head best)))
