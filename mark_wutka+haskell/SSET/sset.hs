import System.IO
import System.Environment
import Text.Printf
import Data.List

subsetCount :: Integer -> Integer
subsetCount n = (2 ^ n) `mod` (toInteger 1000000)

main = do
  argv <- getArgs
  fileData <- readFile (head argv)
  let n = read fileData
  putStrLn . show $ subsetCount n
