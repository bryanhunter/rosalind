import System.IO
import System.Environment
import Text.Printf
import Data.List
import Roselib.FASTA
import Control.Monad.Memo

longestCommonSubsequence :: (String,String) -> Memo (String,String) String String
longestCommonSubsequence ([],_) = return []
longestCommonSubsequence (_,[]) = return []
longestCommonSubsequence ((s:ss),(t:ts)) =
    if s == t then
        (memo longestCommonSubsequence (ss,ts)) >>= (\l -> return (s:l))
    else
        do
          l1 <- memo longestCommonSubsequence (ss,(t:ts))
          l2 <- memo longestCommonSubsequence ((s:ss),ts)
          if length l1 > length l2 then
              return l1
          else
              return l2

main = do
  argv <- getArgs
  fileData <- readFile (head argv)
  let fasta = parseFASTA $ lines fileData
  let fasta_DNA = map dna fasta
  let (s:t:_) = fasta_DNA
  putStrLn $ startEvalMemo $ longestCommonSubsequence (s,t)
