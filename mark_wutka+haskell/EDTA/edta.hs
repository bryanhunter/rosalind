import System.IO
import System.Environment
import Text.Printf
import Data.List
import Roselib.FASTA
import Control.Monad.Memo
import Debug.Trace

minDist (ax,as,at) (bx,bs,bt) =
    if ax < bx then (ax,as,at) else (bx, bs, bt)

editDistance :: (String,String) -> Memo (String,String) (Int,String,String) (Int,String,String)
editDistance(s,[]) = return $ (length s, s, replicate (length s) '-')
editDistance([],t) = return $ (length t, replicate (length t) '-', t)
editDistance (s:ss,t:ts) =
    if s == t then do
      (bx, bxs, bxt) <- memo editDistance (ss, ts)
      return (bx, s:bxs, t:bxt)
    else do
      (sx,sxs,sxt) <- memo editDistance (ss,t:ts)
      let sx' = (sx+1, s:sxs, '-':sxt)
      (tx, txs, txt) <- memo editDistance (s:ss,ts)
      let tx' = (tx+1, '-':txs, t:txt)
      (bx, bxs, bxt) <- memo editDistance (ss,ts)
      let bx' = (bx+1, s:bxs, t:bxt)
      
      return $ minDist sx' (minDist tx' bx')

main = do
  argv <- getArgs
  fileData <- readFile (head argv)
  let fasta = parseFASTA $ lines fileData
  let fasta_DNA = map dna fasta
  let (s:t:_) = fasta_DNA
  let (bestx, bests, bestt) = startEvalMemo $ editDistance (s,t)
  putStrLn . show $ bestx
  putStrLn bests
  putStrLn bestt
