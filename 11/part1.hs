import System.IO
import Data.Char (digitToInt)

myIndexedArray l = foldl (\acc x -> (foldl (\acc2 y -> (x,y):acc2) [] (reverse [0..(length l)-1])):acc) [] (reverse [0..(length (head l))-1])

getFlashValue :: [[Int]] -> (Int, Int) -> [(Int, Int)] -> Int
getFlashValue l (x,y) flashed = inc
    where inc = fromEnum (elem (x+1,y) flashed) + 
                fromEnum (elem (x-1,y) flashed) + 
                fromEnum (elem (x,y+1) flashed) + 
                fromEnum (elem (x,y-1) flashed) +
                fromEnum (elem (x+1,y+1) flashed) +
                fromEnum (elem (x+1,y-1) flashed) +
                fromEnum (elem (x-1,y+1) flashed) +
                fromEnum (elem (x-1,y-1) flashed)

-- This also flips array... not a problem probably
increaseValue :: [[Int]] -> [[Int]]
increaseValue flashed = foldl (\acc x -> foldl (\acc2 y -> y+1:acc2) [] x:acc) [] flashed

-- Get ones that flasehd first
getSimpleFlashed :: [[Int]] -> [(Int, Int)]
getSimpleFlashed l = filter (\(x,y) -> l!!x!!y > 9) [(x,y) | x <- [0..length l-1], y <- [0..length (head l)-1]]

-- Check if flashed based on neighbours
checkOne :: [[Int]] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
checkOne l (x,y) flashed
    | x < 0 || y < 0 || x >= length l || y >= length (head l) = []
    | elem (x,y) flashed = [] -- TODO
    | (l!!x!!y) + inc > 9 = [(x,y)]
    | otherwise = [] -- TODO
    where inc = fromEnum (elem (x+1,y) flashed) + 
                fromEnum (elem (x-1,y) flashed) + 
                fromEnum (elem (x,y+1) flashed) + 
                fromEnum (elem (x,y-1) flashed) +
                fromEnum (elem (x+1,y+1) flashed) +
                fromEnum (elem (x+1,y-1) flashed) +
                fromEnum (elem (x-1,y+1) flashed) +
                fromEnum (elem (x-1,y-1) flashed)

-- Recursively check new flashes
-- array, flashed, newFlashesToCheck
checkNeighbours :: [[Int]] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
checkNeighbours _ flashed [] = flashed
checkNeighbours l flashed newFlashes = checkNeighbours l (flashed++newerFlashes) newerFlashes
    where neighbours = foldl (++) [] (map (\(x,y) -> [(i,j) | i <- [x-1..x+1], j <- [y-1..y+1]]) newFlashes)
          newerFlashes = foldl (\acc n -> (checkOne l n (flashed++acc))++acc) [] neighbours

-- Also flips array... should not be a problem
resetFlashed :: [[Int]] -> [(Int, Int)] -> [[Int]]
resetFlashed l flashed = foldl (\acc x -> (foldl (\acc2 (i,j) -> case () of _ | elem (i,j) flashed -> 0:acc2 | otherwise -> (l!!i!!j)+(getFlashValue l (i,j) flashed):acc2) [] x):acc) [] (myIndexedArray l)

solve :: [[Int]] -> Int -> Int
solve _ 0 = 0
solve l steps = length allFlashed + solve (resetFlashed increased allFlashed) (steps - 1)
    where increased = increaseValue l
          flashed = getSimpleFlashed increased
          allFlashed = checkNeighbours increased flashed flashed



main = do
        let list = []
        handle <- openFile "input_test.txt" ReadMode
        contents <- hGetContents handle
        --print (lines contents)
        let l = lines contents
        let parsedInput = parseInput l
        
        
        print l
        print $ parseInput l
        print $ increaseValue parsedInput
        print $ getSimpleFlashed $ increaseValue parsedInput
        let increased = increaseValue parsedInput
        let flashed = getSimpleFlashed increased
        print increased
        print flashed
        print $ checkNeighbours increased flashed flashed
        print $ solve parsedInput 100


parseInput :: [String] -> [[Int]]
parseInput = map $ map digitToInt