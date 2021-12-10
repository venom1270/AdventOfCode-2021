import System.IO
import Data.Char (digitToInt)
import Data.Set (fromList)
import Data.List (sort)
import Control.Monad.State

getUp :: [[Int]] -> Int -> Int -> Int 
getUp l x y 
    | x == 0 = -1
    | otherwise = l !! (x-1) !! y 
        
getDown :: [[Int]] -> Int -> Int -> Int 
getDown l x y 
    | x == n = -1
    | otherwise = l !! (x+1) !! y 
    where n = length l - 1

getLeft :: [[Int]] -> Int -> Int -> Int 
getLeft l x y 
    | y == 0 = -1
    | otherwise = l !! x !! (y-1) 


getRight :: [[Int]] -> Int -> Int -> Int 
getRight l x y 
    | y == m = -1
    | otherwise = l !! x !! (y+1) 
    where m = length (head l) - 1

getLowPoints :: [[Int]] -> [(Int, Int)]
getLowPoints l = filter (\(i,j) -> minimum (filter (/=(-1)) [getUp l i j, getDown l i j, getLeft l i j, getRight l i j]) > l!!i!!j) [(i,j) | i <- [0..n], j <- [0..m]]
    where n = length l - 1
          m = length (head l) - 1


getBasinSize :: [[Int]] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)] 
getBasinSize l (x,y) visited
    | elem (x,y) visited = visited
    | x < 0 || y < 0 || x >= length l || y >= length (head l) = visited
    | l !! x !! y /= 9 = do
             let v1 = getBasinSize l (x+1,y) ((x,y):visited)
             let v2 = getBasinSize l (x-1,y) v1
             let v3 = getBasinSize l (x,y+1) v2 
             let v4 = getBasinSize l (x,y-1) v3
             v4
    | otherwise = visited

multiplyTopThree :: [Int] -> Int 
multiplyTopThree (h1:(h2:h3:t)) = h1*h2*h3
multiplyTopThree _ = 0

main = do
        let list = []
        handle <- openFile "input_test.txt" ReadMode
        contents <- hGetContents handle
        --print (lines contents)
        let parsedInput = parseInput (lines contents)
        let l = parsedInput
        print parsedInput
        print $ getLowPoints parsedInput
        --print (fromList (getBasinSize parsedInput (0,9) []))
        --print $ getBasinSize parsedInput (0,1) []
        --print $ map (\(x,y) -> length (fromList (getBasinSize parsedInput (x,y) []))) (getLowPoints parsedInput) 
        print $ multiplyTopThree $ reverse $ sort $ map (\(x,y) -> length (fromList (getBasinSize parsedInput (x,y) []))) (getLowPoints parsedInput) 




parseInput :: [String] -> [[Int]]
parseInput = map $ map digitToInt
