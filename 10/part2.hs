import System.IO
import Data.List

score :: Char -> Int
score c
    | c == ')' = 1
    | c == ']' = 2
    | c == '}' = 3
    | c == '>' = 4
    | otherwise = 0

counter :: Char -> Char
counter c
    | c == '(' = ')'
    | c == '[' = ']'
    | c == '{' = '}'
    | c == '<' = '>'
    | c == ')' = '('
    | c == ']' = '['
    | c == '}' = '{'
    | c == '>' = '<'
    | otherwise = '0'

checkCorrupted :: String -> String -> String
checkCorrupted [] s = s
checkCorrupted (h:t) s
    | opening = checkCorrupted t (counter h:s)
    | closing && h == head s = checkCorrupted t (tail s)
    | otherwise = []
    where opening = h == '(' || h == '[' || h == '<' || h == '{'
          closing = h == ')' || h == ']' || h == '>' || h == '}'

calculateScore :: String -> Int
calculateScore = foldl (\acc c -> 5*acc + score c) 0

getMiddleEl :: [Int] -> Int
getMiddleEl l = sort filteredList !! mid
    where filteredList = filter (>0) l
          mid = length filteredList `div` 2

main = do
        let list = []
        handle <- openFile "input_test.txt" ReadMode
        contents <- hGetContents handle
        --print (lines contents)
        let l = lines contents
        print $ map (`checkCorrupted` []) l
        print $ getMiddleEl (map calculateScore (map (`checkCorrupted` []) l))


