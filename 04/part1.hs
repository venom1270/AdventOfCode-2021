import System.IO
import Control.Monad
import GHC.Read (list)
import Debug.Trace
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Text (splitOn, replace)


bingoBoardMarkNumber :: [Int] -> Int -> [Int]
bingoBoardMarkNumber [] _ = []
bingoBoardMarkNumber (h:t) n
        | h == n = -1 : bingoBoardMarkNumber t n
        | otherwise = h : bingoBoardMarkNumber t n

bingoBoardsMarkNumbers :: [[Int]] -> Int -> [[Int]]
bingoBoardsMarkNumbers x n = map (`bingoBoardMarkNumber` n) x

--bingoBoardCheck :: [Int] -> Bool 
--bingoBoardCheck 

bingoGame :: [[Int]] -> [Int] -> Int
bingoGame boards numbersList
        | any (==True) check = number * sum (filter (>= 0) (markedBoards !! fromMaybe 999999 (elemIndex True check))) -- 99999 is default value - should never happen
        | null numbers = 0
        | otherwise = bingoGame markedBoards numbers
        where markedBoards = bingoBoardsMarkNumbers boards number
              check = map checkBoard markedBoards
              number = head numbersList
              numbers = tail numbersList

checkBoard :: [Int] -> Bool
checkBoard x = checkRows x || checkCols x 0

checkRows :: [Int] -> Bool
checkRows [] = False
checkRows x = all (== -1) (take 5 x) || checkRows (drop 5 x)

checkCols :: [Int] -> Int -> Bool
checkCols _ 5 = False
checkCols x n = all (== -1) (everyNth x n) || checkCols x (n+1)

everyNth :: [Int] -> Int -> [Int]
everyNth [] _ = []
everyNth x n = take 5 x !! n : everyNth (drop 5 x) n

main = do
        let list = []
        handle <- openFile "input_test.txt" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
        let drawnNumbers = readDrawnNumbers (head singlewords)
        let bingoNumbersIntList = stringListToInt (tail singlewords)
        let bingoBoards = toBingoBoards bingoNumbersIntList 5
        --print (checkCols (head bingoBoards) 5)
        print drawnNumbers
        print bingoNumbersIntList
        print (bingoGame bingoBoards drawnNumbers)


readDrawnNumbers :: String -> [Int]
readDrawnNumbers x = map read (split ',' x)

inputToTuples :: [String] -> [Int]
inputToTuples (h1:t) = read h1: inputToTuples t
inputToTuples _ = []

stringListToInt :: [String] -> [Int]
stringListToInt = map read

toBingoBoards :: [Int] -> Int -> [[Int]]
toBingoBoards [] _ = []
toBingoBoards x size = take (size*size) x : toBingoBoards (drop (size*size) x) size

-- From stackoverflow
split :: Char -> String -> [String]
split _ "" = []
split c s = firstWord : (split c rest)
    where firstWord = takeWhile (/=c) s
          rest = drop (length firstWord + 1) s

{-
toBingoBoardArrays :: [Int] -> [[[Int]]]
toBingoBoardArrays x boardSize = toBingoBoard (take boardSize*boardSize x) boardSize : toBingoBoardArrays (drop boardSize*boardSize x) boardSize

toBingoBoard :: [Int] -> Int -> [[Int]]
toBingoBoard [] _ = []
toBingoBoard x size = drop size x : toBingoBoard (drop size x) size
-}