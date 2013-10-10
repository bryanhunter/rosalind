import System.IO
import System.Environment
import Text.Printf
import Data.List
import Data.Bits
import Roselib.Combinations

signedList :: [Int] -> Int -> [Int]
signedList [] _ = []
signedList (x:xs) i =
   xval : signedList xs ival
   where
        xval = if i `mod` 2 == 1 then -x else x
        ival = i `div` 2

signedPermutations :: Int -> [[String]]
signedPermutations n =
    [ map show $ signedList p i | i <- [0 .. ((shiftL 1 n) - 1)],
                       p <- permutations [1 .. n] ]
 
readInt :: String -> Int
readInt = read

main = do
  argv <- getArgs
  fileLines <- readFile (head argv)
  let n = readInt fileLines
  putStrLn $ show ((factorial $ toInteger n) * (shiftL 1 n))
  putStr . unlines . map unwords $ (signedPermutations n)