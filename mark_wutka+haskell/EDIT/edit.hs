import System.IO
import System.Environment
import Text.Printf
import Data.List
import Roselib.FASTA
import Control.Monad.Memo

editDistance :: (String,String) -> Memo (String,String) Int Int
editDistance(s,[]) = return $ length s
editDistance([],t) = return $ length t
editDistance (s:ss,t:ts) =
    if s == t then
        memo editDistance (ss, ts)
    else do
      sx <- memo editDistance (ss,t:ts)
      tx <- memo editDistance (s:ss,ts)
      stx <- memo editDistance (ss,ts)
      return $ 1 + (min sx (min tx stx))

main = do
  argv <- getArgs
  fileData <- readFile (head argv)
  let fasta = parseFASTA $ lines fileData
  let fasta_DNA = map dna fasta
  let (s:t:_) = fasta_DNA
  putStrLn . show $ startEvalMemo $ editDistance (s,t)
