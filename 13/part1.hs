import System.IO
import Data.Char (isUpper)


foldPaper :: [(Int, Int)] -> [(Char, Int)] -> [(Int, Int)]
foldPaper points [] = points
foldPaper points (fold:t) =
    foldPaper (foldl (\acc (x,y) -> (makeFold acc (x,y) fold)) [] points) t
    where dir = fst fold
          foldIndex = snd fold

makeFold :: [(Int, Int)] -> (Int, Int) -> (Char, Int) -> [(Int, Int)]
makeFold points (x,y) (dir, foldIndex)
    | dir == 'y' && y > foldIndex && notElem newY points = newY:points
    | dir == 'x' && x > foldIndex && notElem newX points = newX:points
    | dir == 'y' && y > foldIndex && elem newY points = points
    | dir == 'x' && x > foldIndex && elem newX points = points
    | elem (x,y) points = points
    -- | (dir == 'x' && foldIndex == x) || (dir == 'y' && foldIndex == y) = points
    | otherwise = (x,y):points
    where newX = (2*foldIndex-x,y)
          newY = (x,2*foldIndex-y)


main = do
        let list = []
        handle <- openFile "input_test.txt" ReadMode
        contents <- hGetContents handle
        --print (lines contents)
        let l = lines contents
        let splitArray = splitInputArray l
        let points = parseInput (fst splitArray)
        print points
        let folds = map parseFold (snd splitArray)
        print folds

        let folded = foldPaper points [folds!!0]
        print folded
        print $ length folded

        let duplicatesCheck = foldl (\acc x -> case () of _ | elem x acc -> acc | otherwise -> x:acc) [] folded

        print $ length duplicatesCheck

        --let folded = foldPaper points folds
        --print folded
        --print $ length folded

        -- let parsedInput = parseInput l

splitInputArray :: [String] -> ([String], [String])
splitInputArray (h:t)
    | h == "" = ([], t)
    | otherwise = (h:fst splitArray, snd splitArray)
    where splitArray = splitInputArray t

parseInput :: [String] -> [(Int, Int)]
parseInput s = map splitToTuple s


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