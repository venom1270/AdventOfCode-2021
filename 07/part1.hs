import System.IO

moveToCost :: Int -> [Int] -> Int
moveToCost pos = foldl (\acc crab -> acc + abs (crab-pos)) 0

getAllCosts :: [Int] -> [Int]
getAllCosts crabs = foldl (\acc pos -> (moveToCost pos crabs):acc) [] [minVal..maxVal]
    where minVal = minimum crabs
          maxVal = maximum crabs

main = do
        let list = []
        handle <- openFile "input_test.txt" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
        let numbers = readNumbers (head singlewords)
        print numbers
        print (minimum (getAllCosts numbers))


readNumbers :: String -> [Int]
readNumbers x = map read (split ',' x)

-- From stackoverflow
split :: Char -> String -> [String]
split _ "" = []
split c s = firstWord : (split c rest)
    where firstWord = takeWhile (/=c) s
          rest = drop (length firstWord + 1) s