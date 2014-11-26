import System.IO
import System.Environment
import Text.Printf
import Data.List
import Roselib.Combinations

combinationSum :: Integer -> Integer -> Integer
combinationSum m n = sum [ choose n k | k <- [m..n]]
    

main = do
  argv <- getArgs
  fileData <- readFile (head argv)
  let (n:m:_) = map read (words fileData)
  putStrLn . show $ (combinationSum m n `mod` (toInteger 1000000))
