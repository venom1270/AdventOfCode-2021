import System.IO

score :: Char -> Int
score c
    | c == ')' = 3
    | c == ']' = 57
    | c == '}' = 1197
    | c == '>' = 25137
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

checkCorrupted :: String -> String -> Char
checkCorrupted [] _ = '0'
checkCorrupted (h:t) s
    | opening = checkCorrupted t (counter h:s)
    | closing && h == head s = checkCorrupted t (tail s)
    | otherwise = h
    where opening = h == '(' || h == '[' || h == '<' || h == '{'
          closing = h == ')' || h == ']' || h == '>' || h == '}'

calculateScore :: String -> Int
calculateScore = foldl (\acc c -> acc + score c) 0

main = do
        let list = []
        handle <- openFile "input_test.txt" ReadMode
        contents <- hGetContents handle
        --print (lines contents)
        let l = lines contents
        print $ map (`checkCorrupted` []) l
        print $ calculateScore $ map (`checkCorrupted` []) l


