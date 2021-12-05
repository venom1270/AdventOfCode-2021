import System.IO
import Control.Monad
import GHC.Read (list)
import Debug.Trace
import Data.List (elemIndex)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (splitOn, replace)

--x1,y1,x2,y2
type Line = (Int, Int, Int, Int)
--x,y,count
type Point = (Int, Int, Int)

filterDiagonals :: [Line] -> [Line]
filterDiagonals = filter (\(x1,y1,x2,y2) -> x1==x2 || y1==y2)

getSolution :: [Point] -> Int
getSolution points = length (filter (\(_,_,c) -> c >= 2) points)

markPointsOnLines :: [Line] -> [Point]
markPointsOnLines lines = markPointsOnLines2 lines []
        where
        markPointsOnLines2 [] points = points
        markPointsOnLines2 (line:t) points = markPointsOnLines2 t (markPoints (getLinePoints line) points)


getLinePoints :: Line -> [Point]
getLinePoints (x1,y1,x2,y2)
        | x1 == x2 && y1 == y2 = [(x1,y1,0)]
        | x1 < x2 = point : getLinePoints (x1+1,y1,x2,y2)
        | x1 > x2 = point : getLinePoints (x1-1,y1,x2,y2)
        | y1 < y2 = point : getLinePoints (x1,y1+1,x2,y2)
        | y1 > y2 = point : getLinePoints (x1,y1-1,x2,y2)
        | otherwise = [] -- should not happen
        where point = (x1,y1,0)

-- points to mark, all points, return list
markPoints :: [Point] -> [Point] -> [Point]
markPoints t points
  = foldl markPoint points (map (\(x,y,_) -> (x,y)) t)

markPoint :: [Point] -> (Int, Int) -> [Point]
markPoint [] (x,y) = [(x,y,1)]
markPoint ((x1,y1,c):t) (x2,y2)
        | x1 == x2 && y1 == y2 = (x1,y1,c+1):t
        | otherwise = (x1,y1,c) : markPoint t (x2,y2)

findPoint :: [Point] -> (Int, Int) -> Maybe Point
findPoint [] _ = Nothing
findPoint ((x1,y1,c):t) (x2,y2)
        | x1 == x2 && y1 == y2 = Just (x1,y1,c)
        | otherwise  = findPoint t (x2,y2)

-- RESULT: 7468
-- Took about an hour hahah
main = do
        let list = []
        handle <- openFile "input_test.txt" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
        print singlewords
        let tuples = inputToTuples singlewords
        print tuples
        print (filterDiagonals tuples)
        let maxVal = maximum (map (\(x1,y1,x2,y2) -> maximum [x1,y1,x2,y2]) tuples)
        print maxVal
        print (getLinePoints (head tuples))
        print (getSolution (markPointsOnLines (filterDiagonals tuples)))


inputToTuples :: [String] -> [Line]
inputToTuples [] = []
inputToTuples (x:_:y:t) = (split1 !! 0, split1 !! 1, split2 !! 0, split2 !! 1) : inputToTuples t
        where split1 = split ',' x
              split2 = split ',' y

-- From stackoverflow
split :: Char -> String -> [Int]
split _ "" = []
split c s = read firstWord : (split c rest)
    where firstWord = takeWhile (/=c) s
          rest = drop (length firstWord + 1) s

{-
toBingoBoardArrays :: [Int] -> [[[Int]]]
toBingoBoardArrays x boardSize = toBingoBoard (take boardSize*boardSize x) boardSize : toBingoBoardArrays (drop boardSize*boardSize x) boardSize

toBingoBoard :: [Int] -> Int -> [[Int]]
toBingoBoard [] _ = []
toBingoBoard x size = drop size x : toBingoBoard (drop size x) size
-}