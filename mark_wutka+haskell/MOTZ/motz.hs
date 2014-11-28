import System.IO
import System.Environment
import Text.Printf
import Data.List
import qualified Data.Map as Map
import Data.Array
import Roselib.FASTA
import Control.Monad.Memo

canPair :: Char -> Char -> Bool
canPair 'A' 'U' = True
canPair 'U' 'A' = True
canPair 'C' 'G' = True
canPair 'G' 'C' = True
canPair _ _ = False

countItems :: Array Int Char -> Int -> Int -> Map.Map Char Integer
countItems a start end =
    Map.fromListWith (+) [ (a ! i, (toInteger 1)) | i <- [start..end] ]

countsOkay :: Array Int Char -> Int -> Int -> Bool
countsOkay a start end =
    (counts Map.! 'A' == counts Map.! 'U') &&
    (counts Map.! 'C' == counts Map.! 'G')
        where
          counts = countItems a start end

countPairings :: Array Int Char -> (Int,Int) -> Memo (Int,Int) Integer Integer
countPairings a (start,end)
    | start >= end = return (toInteger 1)
    | otherwise =
        foldM combinePairings (toInteger 1) (filter pairOk pairs)
            where
              combinePairings :: Integer -> (Int,Int) -> Memo (Int,Int) Integer Integer
              combinePairings acc (i,j) = do
                m1 <-memo (countPairings a) (i+1, j-1)
                m2 <- memo (countPairings a) (j+1, end)
                return (acc + (m1 * m2))
              pairs = [(i,j) | i <- [start..(end-1)], j <- [(i+1)..end]]
              pairOk (i,j) = canPair (a ! i) (a ! j)

main = do
  argv <- getArgs
  fileData <- readFile (head argv)
  let fasta = parseFASTA $ lines fileData
  let fasta_DNA = map dna fasta
  let s = head fasta_DNA
  let a = listArray (0,(length s) - 1) s
  putStrLn . show $ (startEvalMemo (countPairings a (0,(length s)-1))) `mod` 1000000
