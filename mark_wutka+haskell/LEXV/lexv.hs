import System.IO
import System.Environment
import Text.Printf
import Data.List

generateStrings :: String -> Int -> [String]
generateStrings _ 0 = [""]
generateStrings charSet maxLen =
    [ c:s | c <- charSet, s <- ("":generateStrings charSet (maxLen-1))]

main = do
  argv <- getArgs
  fileData <- readFile (head argv)
  let (charLine:countLine:_) = lines fileData
  let charSet = concat $ words charLine
  let maxLen = read countLine
  putStrLn . unlines $ nub $ generateStrings charSet maxLen
