import System.IO  
import Control.Monad
import GHC.Read (list)
import GHCi.Message (EvalOpts(singleStep))


calculateGERate :: [String] -> [Int] -> [Int] 
calculateGERate [] sumArray = map (\x -> case () of _ | x > 0 -> 1 | otherwise -> 0) sumArray -- I could probably return final result here but decided not to, for clarity's sake ;)
calculateGERate (s:t) sumArray = calculateGERate t (map (\(x,y) -> case () of _ | x == '1' -> y+1 | otherwise -> y-1) (zip s sumArray))

binToInt :: [Int] -> Int 
binToInt [] = 0
binToInt (x : xs) = x + 2 * binToInt xs

main = do  
        let list = []
        handle <- openFile "input_test.txt" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
        print singlewords

        --print (map (\(x,y) -> case () of _ | x == '1' -> y+1 | otherwise -> y-1) (zip (head list) [0,0,0,0]))
        let gammaBin = calculateGERate singlewords (replicate (length (head singlewords)) 0)
        let epsilonBin = map (\x -> case () of _ | x == 1 -> 0 | otherwise -> 1) gammaBin
        print gammaBin
        print epsilonBin
        print (binToInt (reverse gammaBin) * binToInt (reverse epsilonBin))



--inputToTuples :: [String] -> [Int]
--inputToTuples (h1:t) = read h1: inputToTuples t
--inputToTuples _ = []