import System.IO  
import Control.Monad
import GHC.Read (list)
import GHCi.Message (EvalOpts(singleStep))
import Data.Char (digitToInt)
import Data.List (foldl')
import Control.Arrow (Arrow(second))


calculateGERate :: [String] -> [Int] -> [Int] 
calculateGERate [] sumArray = map (\x -> case () of _ | x > 0 -> 1 | otherwise -> 0) sumArray
calculateGERate (s:t) sumArray = calculateGERate t (map (\(x,y) -> case () of _ | x == '1' -> y+1 | otherwise -> y-1) (zip s sumArray))

negateBinaryInt :: Int -> Int 
negateBinaryInt x 
        | x >= 1 = 0
        | otherwise = 1

mostCommon :: [String] -> Int -> Int
mostCommon [] sumVar = head (map (\x -> case () of _ | x >= 0 -> 1 | otherwise -> 0) [sumVar]) -- CHEATING!! :P
mostCommon (h:t) sumVar
        | head h == '1' = mostCommon t (sumVar+1)
        | otherwise = mostCommon t (sumVar-1)

leastCommon :: [String] -> Int -> Int
leastCommon [] sumVar = head (map (\x -> case () of _ | x > 0 -> 1 | otherwise -> 0) [sumVar]) -- CHEATING!! :P
leastCommon (h:t) sumVar
        | head h == '1' = leastCommon t (sumVar-1)
        | otherwise = leastCommon t (sumVar+1)


-- [(FullList, IndexedList)], SumArray -> Final result
calculateOCRate :: [(String, String)] -> ([String] -> Int -> Int) -> Int
calculateOCRate list f
        | null list = 0
        | length list == 1 = toDec (fst (head list))
        | null (snd (head list)) = sum (map (\(x,_) -> toDec x) list)
        | otherwise = calculateOCRate (map (\(i,j) -> (i, tail j)) (filter (\(_,y) -> digitToInt (head y) == (f (map snd list) 0)) list)) f

binArrayToInt :: [Int] -> Int 
binArrayToInt [] = 0
binArrayToInt (x : xs) = x + 2 * binArrayToInt xs

-- This is copied from somewhere else...
toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

main = do  
        let list = []
        handle <- openFile "input_test.txt" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
        print singlewords
        --print (map (\(x,y) -> case () of _ | x == '1' -> y+1 | otherwise -> y-1) (zip (head list) [0,0,0,0]))
        let gammaBin = calculateGERate singlewords (replicate (length (head singlewords)) 0)
        let epsilonBin = map (\x -> case () of _ | x == 1 -> 0 | otherwise -> 1) gammaBin
        --print gammaBin
        --print epsilonBin
        --print (binArrayToInt (reverse gammaBin) * binArrayToInt (reverse epsilonBin))
        --print (calculateOCRate (zip singlewords singlewords) mostCommon)

        print "-----"
        print "RESULT"
        print (calculateOCRate (zip singlewords singlewords) mostCommon * calculateOCRate (zip singlewords singlewords) leastCommon)
        print "-----"
        --print (head singlewords)
        --print (head (head singlewords))
        --print (digitToInt (head (head singlewords)))
        --print epsilonBin
        --print (filter (\(_,y) -> digitToInt (head y) == head epsilonBin) (zip singlewords singlewords))
        --print (map (\(i,j) -> (i, tail j)) (filter (\(_,y) -> digitToInt (head y) == head epsilonBin) (zip singlewords singlewords)))
        --print (sum (map (\(x,_) -> toDec x) (zip ["100","111","001"]  ["100","111","001"])))
        --print (leastCommon ["10"] 0)
        --print (calculateBits ["100","111","001"] 0)

--inputToTuples :: [String] -> [Int]
--inputToTuples (h1:t) = read h1: inputToTuples t
--inputToTuples _ = []