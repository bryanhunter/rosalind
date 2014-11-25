import System.IO
import System.Environment
import Text.Printf
import Data.List
import qualified Data.Map as Map
import Data.Array
import Roselib.FASTA
import Control.Monad
import Control.Monad.Memo

canPair :: Char -> Char -> Bool
canPair c1 c2 = 
    ((c1 == 'A') && (c2 == 'U')) ||
    ((c1 == 'U') && (c2 == 'A')) ||
    ((c1 == 'C') && (c2 == 'G')) ||
    ((c1 == 'G') && (c2 == 'C'))

countItems :: Array Int Char -> Int -> Int -> Map.Map Char Integer
countItems rna start end = Map.fromListWith (+) [(rna ! i,(toInteger 1)) | i <- [start .. end]]

pairCountOkay :: Map.Map Char Integer -> Char -> Char -> Bool
pairCountOkay counts ch1 ch2 =
    if Map.member ch1 counts then
        if Map.member ch2 counts then
            (counts Map.! ch1) == (counts Map.! ch2)
        else
            False
    else
        Map.notMember ch2 counts

countsOkay rna start end =
    (pairCountOkay counts 'A' 'U') && (pairCountOkay counts 'C' 'G')
    where
        counts = countItems rna start end

countPairings :: Array Int Char -> Int -> Int -> Integer
countPairings rna start end =
    if start > end then
        1
    else if start == end then
        0
    else if not $ countsOkay rna start end then
        0
    else
        sum (map countPairingsForPair $ (filter pairOk [(start, i) | i <- [start+1..end]]))
    where
        countPairingsForPair (i,j) = (countPairings rna (i+1) (j-1)) * (countPairings rna (j+1) end)
        pairOk (i,j) = canPair (rna ! i) (rna ! j)

-- Use Memoization monad to cache results
countPairingsm :: Array Int Char -> (Int, Int) -> Memo (Int,Int) Integer Integer
countPairingsm rna (start,end) =
    if start > end then
        return 1
    else if start == end then
        return 0
    else if not $ countsOkay rna start end then
        return 0
    else
        liftM sum $ mapM countPairingsForPair $ filter pairOk [(start, i) | i <- [start+1..end]]
    where
        countPairingsForPair (i,j) = do
            n1 <- memo (countPairingsm rna) (i+1,j-1)
            n2 <- memo (countPairingsm rna) (j+1,end)
            return (n1 * n2)
        pairOk (i,j) = canPair (rna ! i) (rna ! j)        

main = do
  argv <- getArgs
  fileData <- readFile (head argv)
  let rna = dna . head . parseFASTA $ lines fileData
  let rnaArray = listArray (0, (length rna)-1) rna
  putStrLn . show $ (startEvalMemo (countPairingsm rnaArray (0,(length rna) - 1))) `mod` 1000000
