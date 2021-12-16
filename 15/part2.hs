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

keepWithLessCost :: [(Int, Int, Int)] -> (Int, Int, Int) -> [(Int, Int, Int)]
keepWithLessCost [] _ = []
keepWithLessCost ((x1,y1,c1):t) (x,y,c)
    | x1 == x && y1 == y && c1 <= c = (x1,y1,c1):t
    | x1 == x && y1 == y && c1 > c = (x,y,c):t
    | otherwise = (x1,y1,c1):keepWithLessCost t (x,y,c)

solve2 :: [[Int]] -> [(Int, Int, Int)] -> [(Int, Int)] -> (Int, Int, Int) -> Int
solve2 cavern nodes costs (x,y,cost)
    | x == (n-1) && y == (m-1) = cost
    | otherwise = let newNodes = map (\(i,j) -> (i,j,cost+cavern!!i!!j)) (filter (\(i,j) -> i >= 0 && i < n && j >= 0 && j < m && notElem (i,j) costs) [(x-1,y),(x+1,y),(x,y+1),(x,y-1)])
                      duplicateCheck = map (\(i,j,_) -> (i,j)) nodes
                      updatedNodes =  foldl (\acc (i,j,c) -> case () of _ | elem (i,j) duplicateCheck -> keepWithLessCost acc (i,j,c) | otherwise -> (i,j,c):acc) nodes newNodes
                      sortedNodes = sortBy sortNodes updatedNodes-- (filter (\(i,j,_) -> notElem (i,j) costs) updatedNodes)
                      (newCostX, newCostY, _) = head sortedNodes
                  in solve2 cavern (tail sortedNodes) ((newCostX, newCostY):costs) (head sortedNodes)
    where n = length cavern
          m = length (head cavern)

sortNodes (i1,j1,c1) (i2,j2,c2)
  | c1 + (500-i1) + (500-j1) <= c2 + (500-i2) + (500-j2) = LT
  -- | c1 <= c2 = LT
  | otherwise = GT

main = do
        let list = []
        handle <- openFile "input_test.txt" ReadMode
        contents <- hGetContents handle
        let table = parseTable (lines contents)
        print table

        let extendedTable = extendTableHeight (extendTableWidth table) 0

        print extendedTable

        let n = length extendedTable
        let m = length (head extendedTable)
        let costsArray = costTableArray n m
        --let costs = costTable n m
        --let solvedCostTable = solve table costsArray (0,0) 0       
        --print $ length $ modifyCostTable costs (0,0) 123
        --print solvedCostTable

        print $ map (\(i,j) -> (i,j,extendedTable!!i!!j)) (filter (\(i,j) -> i >= 0 && i < n && j >= 0 && j < m) [(0-1,0),(0+1,0),(0,0+1),(0,0-1)])

        let solved2 = solve2 extendedTable [] [] (0,0,0)
        print solved2




        print "qwe"

extendTableWidth :: [[Int]] -> [[Int]]
extendTableWidth [] = []
extendTableWidth (h:t) = extendRow h 0:extendTableWidth t

extendRow :: [Int] -> Int -> [Int]
extendRow row count 
    | count == 5 = []
    | otherwise = map (\x -> case () of _ | x+count > 9 -> x+count-9 | otherwise -> x+count) row ++ extendRow row (count+1)



extendTableHeight :: [[Int]] -> Int -> [[Int]]
extendTableHeight table count
    | count == 5 = []
    | otherwise = map (\x -> map (\y -> case () of _ | y+count > 9 -> y+count-9 | otherwise -> y+count) x) table ++ extendTableHeight table (count+1)



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