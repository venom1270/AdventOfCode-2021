import System.IO
import Data.List (sort)

getTranslationTable :: [String] -> [(Int, String)]
getTranslationTable s = zip [0..9] (map sort t5)
    where t1 = ["",  head (filter (\x->length x == 2) s), "", "", head (filter (\x->length x == 4) s), "", "", head (filter (\x->length x == 3) s), head (filter (\x->length x == 7) s)]
          t2 = ["", t1 !! 1, "", head $ filter (\x->length x == 5 && includes x (t1 !! 1)) s, t1!!4, "", "", t1!!7, t1!!8, ""]
          t3 = ["", t2!!1, "", t2!!3, t2!!4, head $ filter (\x->length x == 5 && x /= (t2!!3) && includes x (diff (t2!!4) (t2!!1))) s, t2!!6, t2!!7, t2!!8, head $ filter (\x->length x == 6 && includes x (t2 !! 3)) s]
          t4 = ["", t3!!1, head $ filter (\x->length x == 5 && x /= t3!!3 && x /= t3!!5) s, t3!!3, t3!!4, t3!!5, head $ filter (\x->length x == 6 && x /= t3!!9 && includes x (t3 !! 5)) s, t3!!7, t3!!8, t3!!9]
          t5 = [head (filter (\x->length x == 6 && x /= t4!!6 && x /= t4!!9) s), t4!!1, t4!!2, t4!!3, t4!!4, t4!!5, t4!!6, t4!!7, t4!!8, t4!!9]

decode :: [String] -> [(Int, String)] -> Int
decode input decodingTable = foldl (\acc x -> acc*10+x) 0 (map (\x -> fst (head (filter (\(_,y) -> sort x==y) decodingTable))) input)

includes :: String -> String -> Bool
includes s inc = foldl (\acc i -> acc && elem i s) True inc

diff :: String -> String -> String
diff s1 s2 = filter (`notElem` s2) s1

main = do
        let list = []
        handle <- openFile "input_test.txt" ReadMode
        contents <- hGetContents handle
        --print (lines contents)
        let parsedInput = parseInput contents
        --print parsedInput
        --print $ fst $ head parsedInput
        --print (getTranslationTable $ fst $ head parsedInput)
        print (sum (map (\(x,y) -> decode y (getTranslationTable x)) parsedInput))



parseInput :: String -> [([String], [String])]
parseInput s = map (\x -> (words (head (split '|' x)), words (head (tail (split '|' x))))) (lines s)

-- From stackoverflow
split :: Char -> String -> [String]
split _ "" = []
split c s = firstWord : (split c rest)
    where firstWord = takeWhile (/=c) s
          rest = drop (length firstWord + 1) s