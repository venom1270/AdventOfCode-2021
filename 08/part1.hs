import System.IO

solve :: [([String], [String])] -> Int
solve s = foldl (\acc x -> acc + length (filter countF x)) 0 (map snd s)

countF :: String -> Bool
countF s
    | l `elem` [2,4,3,7] = True
    | otherwise = False
    where l = length s

main = do
        let list = []
        handle <- openFile "input_test.txt" ReadMode
        contents <- hGetContents handle
        --print (lines contents)
        let parsedInput = parseInput contents
        print parsedInput
        --print (map snd parsedInput)
        --print (countF ((map snd parsedInput !! 1) !! 3))
        print (solve parsedInput)


parseInput :: String -> [([String], [String])]
parseInput s = map (\x -> (words (head (split '|' x)), words (head (tail (split '|' x))))) (lines s)

-- From stackoverflow
split :: Char -> String -> [String]
split _ "" = []
split c s = firstWord : (split c rest)
    where firstWord = takeWhile (/=c) s
          rest = drop (length firstWord + 1) s