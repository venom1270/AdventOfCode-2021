import System.IO
import Data.Char (isUpper)

initPolymer :: [(String, Char)] -> [(String, Int)]
initPolymer [] = []
initPolymer ((x,_):t) = (x, 0):initPolymer t

initValues :: [(String, Int)] -> String -> [(String, Int)]
initValues polymer (_:[]) = polymer
initValues polymer (h1:(h2:t)) = initValues newPolymer (h2:t)
    where newPolymer = increaseValue polymer [[h1,h2]] 1

increaseValue :: [(String, Int)] -> [String] -> Int -> [(String, Int)]
increaseValue polymer sToIncrease val
    | null polymer = []
    | elem (fst (head polymer)) sToIncrease =
    let h = head polymer
        t = tail polymer
    in
        (fst h, snd h + val):increaseValue t sToIncrease val
    | otherwise = (head polymer):increaseValue (tail polymer) sToIncrease val


nextStep :: [(String, Int)] -> [(String, Int)] -> [(String, Char)] -> [(String, Int)]
nextStep oldPolymer newPolymer translationTable
    | null oldPolymer = newPolymer
    | snd (head oldPolymer) == 0 = nextStep (tail oldPolymer) newPolymer translationTable
    | otherwise =
    let h = head oldPolymer
        t = tail oldPolymer
        translation = head $ filter (\(x,_) -> x == fst h) translationTable
        charToInsert = snd translation
        oldString = fst translation
        newString1 = head oldString:[charToInsert]
        newString2 = charToInsert:tail oldString
        val = snd h
        updatedPolymer = increaseValue newPolymer [newString1, newString2] val
    in
        nextStep t updatedPolymer translationTable


countElements :: [(String, Int)] -> [(Char, Int)] -> [(Char, Int)]
countElements els count
    | null els = count
    | elem (head (fst (head els))) (map fst count) = countElements (tail els) (insertEl (head els) count)
    | otherwise = countElements (tail els) ((head (fst (head els)), snd (head els)):count)

insertEl :: (String, Int) -> [(Char, Int)] -> [(Char, Int)]
insertEl el count 
    | null count = []
    | head (fst el) == fst (head count) = (head (fst el), (snd (head count)) + (snd el)):(tail count)
    | otherwise = (head count):insertEl el (tail count)

main = do
        let list = []
        handle <- openFile "input_test.txt" ReadMode
        contents <- hGetContents handle
        --print (lines contents)
        let l = lines contents
        let p = head l
        let translationTable = parseTable (tail (tail l))
        print p
        print (tail (tail l))
        print translationTable
        let blankPolymer = initPolymer translationTable
        let initPolymer = initValues blankPolymer p
        print initPolymer
        print $ nextStep initPolymer blankPolymer translationTable
        let steps=10
        let solve = foldl (\acc _ -> nextStep acc blankPolymer translationTable) initPolymer [1..steps]
        print solve
        let counted = countElements solve []
        let countFixed = countElements [(reverse p, 1)] counted
        print counted
        print countFixed
        print $ (maximum (map snd countFixed)) - (minimum (map snd countFixed))


parseTable :: [String] -> [(String, Char)]
parseTable [] = []
parseTable (h:t) = (s3!!0, head (s2!!1)) : (parseTable t)
    where s = split '-' h
          s2 = split ' ' (s!!1)
          s3 = split ' ' (s!!0)


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