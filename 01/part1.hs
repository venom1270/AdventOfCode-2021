import System.IO  
import Control.Monad
import GHC.Read (list)


countIncrease :: [Int] -> Int 
countIncrease [] = 0
countIncrease (h:t)
  | null t = 0
  | h < head t = 1 + countIncrease t
  | otherwise = countIncrease t

main = do  
        let list = []
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
            list = f singlewords
        print list
        --print (head list)
        print (countIncrease list)

f :: [String] -> [Int]
f = map read