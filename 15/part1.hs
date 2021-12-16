import System.IO
import Data.Char (isUpper, digitToInt)
import Data.Array ((//), Array, (!), listArray)
import Data.List (sortBy)


costTableArray n m = listArray ((0,0), (n-1, m-1)) (replicate (m*n) 999999999) --replicate n . replicate m $ 999999999

costTable n m = replicate n . replicate m $ 999999999

replaceInList :: [Int] -> Int -> Int -> Int -> [Int]
replaceInList l el curPos pos
    | null l = []
    | pos == curPos = el:tail l
    | otherwise = head l:replaceInList (tail l) el (curPos+1) pos

replaceIn2DList :: [[Int]] -> (Int, Int) -> Int -> Int -> [[Int]]
replaceIn2DList l (x,y) el curPos
    | null l = []
    | x == curPos = replaceInList (head l) el 0 y:tail l
    | otherwise = head l:replaceIn2DList (tail l) (x,y) el (curPos+1)

--modifyCostTable :: [[Int]] -> (Int, Int) -> Int -> [[Int]]
--modifyCostTable table pos el = replaceIn2DList table pos el 0
modifyCostTable :: Array (Int, Int) Int -> (Int, Int) -> Int -> Array (Int, Int) Int
modifyCostTable table pos el = table // [(pos, el)]


solve :: [[Int]] -> Array (Int, Int) Int -> (Int, Int) -> Int -> Array (Int, Int) Int
solve cavern costs (x,y) currentCost
    | x < 0 || x >= n || y < 0 || y >= m = costs
    | costs!(x,y) <= currentCost = costs
    | otherwise = let modifiedCosts = modifyCostTable costs (x,y) currentCost
                  in foldl (\acc nextPos -> solve cavern acc nextPos (currentCost+(cavern!!x!!y))) modifiedCosts [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
    where n = length cavern
          m = length (head cavern)



solve2 :: [[Int]] -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> (Int, Int, Int) -> Int
solve2 cavern nodes costs (x,y,cost)
    | x == (n-1) && y == (m-1) = cost
    | otherwise = let newNodes = map (\(i,j) -> (i,j,cost+cavern!!i!!j)) (filter (\(i,j) -> i >= 0 && i < n && j >= 0 && j < m) [(x-1,y),(x+1,y),(x,y+1),(x,y-1)])
                      duplicateCheck = map (\(i,j,_) -> (i,j)) nodes
                      updatedNodes =  foldl (\acc (i,j,c) -> case () of _ | elem (i,j) duplicateCheck -> acc | otherwise -> (i,j,c):acc) nodes newNodes
                      mappedCosts = map (\(i,j,_) -> (i,j)) costs
                      sortedNodes = sortBy sortNodes (filter (\(i,j,_) -> notElem (i,j) mappedCosts) updatedNodes)
                  in solve2 cavern (tail sortedNodes) (head sortedNodes:costs) (head sortedNodes)
    where n = length cavern
          m = length (head cavern)

sortNodes (_,_,c1) (_,_,c2)
  | c1 <= c2 = LT
  | otherwise = GT

main = do
        let list = []
        handle <- openFile "input_test.txt" ReadMode
        contents <- hGetContents handle
        let table = parseTable (lines contents)
        print table


        let n = length table
        let m = length (head table)
        let costsArray = costTableArray n m
        --let costs = costTable n m
        --let solvedCostTable = solve table costsArray (0,0) 0       
        --print $ length $ modifyCostTable costs (0,0) 123
        --print solvedCostTable

        print $ map (\(i,j) -> (i,j,table!!i!!j)) (filter (\(i,j) -> i >= 0 && i < n && j >= 0 && j < m) [(0-1,0),(0+1,0),(0,0+1),(0,0-1)])

        let solved2 = solve2 table [] [] (0,0,0)
        print solved2




        print "qwe"


parseTable :: [String] -> [[Int]]
parseTable [] = []
parseTable (h:t) = (reverse (foldl (\acc x -> (digitToInt x) : acc) [] h)) : parseTable t


splitToTuple :: String -> (Int, Int)
splitToTuple s = (read (head splitString), read (head (tail splitString)))
    where splitString = split ',' s

-- From stackoverflow
split :: Char -> String -> [String]
split _ "" = []
split c s = firstWord : (split c rest)
    where firstWord = takeWhile (/=c) s
          rest = drop (length firstWord + 1) s

parseFold :: String -> (Char,Int)
parseFold f = (f!!11, read $ reverse (takeWhile (/='=') (reverse f)))