import System.IO
import Data.Char (isUpper, digitToInt)

substr :: String -> Int -> Int -> String
substr s from to = take (to-from) (drop from s)

binaryToDec :: String -> Int
binaryToDec b = binaryToDecHelper (reverse b) 1 where
    binaryToDecHelper bin p
        | null bin = 0
        | otherwise = (digitToInt (head bin) * p) + binaryToDecHelper (tail bin) (p*2)

parseLiteral :: String -> Int -> (Int, Int)
parseLiteral [] index = (0,index)
parseLiteral s index
    | h == '1' =
        let (l2, i) = parseLiteral (drop 5 s) (index+5)
        in (t + l2, i)
    | h == '0' =
        let newIndex = index + 5
            diff = newIndex `mod` 4
            add = 0
                -- | diff == 0 = 0
                -- | otherwise = (4-diff)
        in (t, newIndex + add)
    | otherwise = (-99999, -99999)
    where literal = substr s 0 5
          h = head literal
          t = binaryToDec $ tail literal

parseAllPackets :: String -> Int -> (Int, Int)
parseAllPackets s index 
    | n-7 <= index = (0, index)
    | otherwise = let (resultString, newIndex) = parsePacket (drop index s)
                      (allResult, newerIndex) = parseAllPackets s (index+newIndex)
                  in (resultString+allResult, newIndex)
    where n = length s

parsePacket :: String -> (Int, Int)
parsePacket [] = (0, 0)
parsePacket s
    | pType == "100" =
        let (literal, index) = parseLiteral (drop 6 s) 6
        in (pVersion, index)
    | pLengthType == "0" =
        let len = binaryToDec (substr s 7 (7+15))
            (ver, i) = parseAllPackets (substr s (7+15) (7+15+len)) 0
        in (ver + pVersion, 7+15+len) -- TODO i or 7+15+len? should be no diff?
    | pLengthType == "1" =
        let len = binaryToDec (substr s 7 (7+11))
            --parsedSubPacket = parseAllPackets (substr s (7+11) (7+11+len)) 0
            parsedSubPacket = foldl (\acc _ -> let (str, i) = parsePacket (drop (snd acc) s)
                                               in ((fst acc)+str, i + (snd acc))) (pVersion, 7+11) [1..len]
        in parsedSubPacket
    | otherwise =  (-99999999, -1)
    where pVersion = binaryToDec $ substr s 0 3
          pType = substr s 3 6
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