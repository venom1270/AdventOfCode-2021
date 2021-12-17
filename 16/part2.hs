import System.IO
import Data.Char (isUpper, digitToInt)

substr :: String -> Int -> Int -> String
substr s from to = take (to-from) (drop from s)

binaryToDec :: String -> Integer
binaryToDec b = binaryToDecHelper (reverse b) 1 where
    binaryToDecHelper bin p
        | null bin = 0
        | otherwise = ((toInteger (digitToInt (head bin))) * p) + binaryToDecHelper (tail bin) (p*2)

parseLiteral :: String -> Int -> (Integer, Int)
parseLiteral [] index = (0,index)
parseLiteral s index = let (bin, i) = parseLiteralHelper s index
                       in (binaryToDec bin, i)

parseLiteralHelper :: String -> Int -> (String, Int)
parseLiteralHelper [] index = ("",index)
parseLiteralHelper s index
    | h == '1' =
        let (l2, i) = parseLiteralHelper (drop 5 s) (index+5)
        in (t ++ l2, i)
    | h == '0' = (t, index + 5)
    | otherwise = ("", -99999)
    where literal = substr s 0 5
          h = head literal
          t = tail literal

-- 1489871 - TOO LOW
parseAllPackets :: String -> Int -> ([Integer], Int)
parseAllPackets s index 
    | n-7 <= index = ([], index)
    | otherwise = let (resultString, newIndex) = parsePacket (drop index s)
                      (allResult, newerIndex) = parseAllPackets s (index+newIndex)
                  in (resultString:allResult, newIndex)
    where n = length s

operator :: Int -> [Integer] -> Integer
operator pType vals
    | pType == 0 = sum vals
    | pType == 1 = product vals
    | pType == 2 = minimum vals
    | pType == 3 = maximum vals
    | pType == 5 = toInteger (fromEnum (vals!!0 > vals!!1))
    | pType == 6 = toInteger (fromEnum (vals!!0 < vals!!1))
    | pType == 7 = toInteger (fromEnum (vals!!0 == vals!!1))
    | otherwise = 0

parsePacket :: String -> (Integer, Int)
parsePacket [] = (0, 0)
parsePacket s
    | pType == 4 =
        let (literal, index) = parseLiteral (drop 6 s) 6
        in (literal, index)
    | pLengthType == "0" =
        let len = fromInteger $ binaryToDec (substr s 7 (7+15))
            (vals, i) = parseAllPackets (substr s (7+15) (7+15+len)) 0
        in (operator pType vals, 7+15+len) -- TODO i or 7+15+len? should be no diff?
    | pLengthType == "1" =
        let len = binaryToDec (substr s 7 (7+11))
            --parsedSubPacket = parseAllPackets (substr s (7+11) (7+11+len)) 0
            (vals, ind) = foldl (\acc _ -> let (str, i) = parsePacket (drop (snd acc) s)
                                               in (str:(fst acc), i + (snd acc))) ([], 7+11) [1..len]
            parsedSubPacket = (operator pType (reverse vals), ind)
        in parsedSubPacket
    | otherwise =  (-99999999, -1)
    where pVersion = binaryToDec $ substr s 0 3
          pType = fromInteger $ binaryToDec $ substr s 3 6
          pLengthType = substr s 6 7

main = do
        let list = []
        handle <- openFile "input_test.txt" ReadMode
        contents <- hGetContents handle
        print contents
        let binary = toBinary contents
        print binary
        let parsed = parseAllPackets binary 0
        --print $ substr "00111000000000000110111101000101001010010001001000000000" (7+15) (7+15+27)
        --print $ parsePacket "110100010100101001000100100"
        --print $ parsePacket (drop 11 "110100010100101001000100100")
        print parsed

        print "qwe"

toBinary [] = []
toBinary (h:t)
    | h == '0' = "0000" ++ toBinary t
    | h == '1' = "0001" ++ toBinary t
    | h == '2' = "0010" ++ toBinary t
    | h == '3' = "0011" ++ toBinary t
    | h == '4' = "0100" ++ toBinary t
    | h == '5' = "0101" ++ toBinary t
    | h == '6' = "0110" ++ toBinary t
    | h == '7' = "0111" ++ toBinary t
    | h == '8' = "1000" ++ toBinary t
    | h == '9' = "1001" ++ toBinary t
    | h == 'A' = "1010" ++ toBinary t
    | h == 'B' = "1011" ++ toBinary t
    | h == 'C' = "1100" ++ toBinary t
    | h == 'D' = "1101" ++ toBinary t
    | h == 'E' = "1110" ++ toBinary t
    | h == 'F' = "1111" ++ toBinary t
    | otherwise = "ERROR"