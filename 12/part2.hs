import System.IO
import Data.Char (isUpper)


constructAdjacencyList :: [(String, String)] -> [(String, [String])] -> [(String, [String])]
constructAdjacencyList t adjList = foldl (flip addToAdjList) adjList t

-- IF
-- node1 already in adjacency list - add to adjacent nodes
-- node1 not in adjacency list - append element in array
addToAdjList :: (String, String) -> [(String, [String])] -> [(String, [String])]
addToAdjList (node1, node2) adjList
    | elem node1 (map fst adjList) = foldl (\acc (node, adjNodes) -> case () of _ | node1 == node -> (node, node2:adjNodes):acc | otherwise -> (node, adjNodes):acc) [] adjList
    | otherwise = (node1, [node2]) : adjList

getAdjacentNodes :: String -> [(String, [String])] -> [String]
getAdjacentNodes node [] = []
getAdjacentNodes node ((n,adjacentNodes):t)
    | node == n = adjacentNodes
    | otherwise = getAdjacentNodes node t



-- currentNode; path ("visited"); adjList; pathsFound
findPaths :: String -> [String] -> [(String, [String])] -> [[String]] -> Bool -> [[String]]
findPaths node path adjList paths canVisitTwice
    | node == "end" = [node:path]
    | not (null path) && node == "start" = []
    | elem node path && isUpper (head node) == False && canVisitTwice == False = []
    | elem node path && isUpper (head node) == False && canVisitTwice == True = foldl (\acc x -> (findPaths x (node:path) adjList paths False) ++ acc) paths (getAdjacentNodes node adjList)
    | otherwise = foldl (\acc x -> (findPaths x (node:path) adjList paths canVisitTwice) ++ acc) paths (getAdjacentNodes node adjList)


main = do
        let list = []
        handle <- openFile "input_test.txt" ReadMode
        contents <- hGetContents handle
        --print (lines contents)
        let l = lines contents
        let parsedInput = parseInput l
        --print parsedInput
        let adjacencyList = constructAdjacencyList parsedInput []
        --print adjacencyList
        let paths = findPaths "start" [] adjacencyList [] True
        --print paths
        print $ length paths


parseInput :: [String] -> [(String, String)]
parseInput s = makeDualWay $ map splitToTuple s

makeDualWay :: [(String, String)] -> [(String, String)]
makeDualWay [] = []
makeDualWay (h:t) = (snd h, fst h) : h : makeDualWay t

splitToTuple :: String -> (String, String)
splitToTuple s = (head splitString, head (tail splitString))
    where splitString = split '-' s

-- From stackoverflow
split :: Char -> String -> [String]
split _ "" = []
split c s = firstWord : (split c rest)
    where firstWord = takeWhile (/=c) s
          rest = drop (length firstWord + 1) s