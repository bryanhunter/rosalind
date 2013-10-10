import System.IO
import System.Environment
import Text.Printf
import Data.List
import Roselib.Combinations

readInt :: String -> Integer
readInt = read

main = do
  argv <- getArgs
  fileLines <- readFile (head argv)
  let [n, k] = map readInt (words fileLines)
  putStrLn $ show ((choose n k) * (factorial k) `mod` 1000000)
