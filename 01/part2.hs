import System.IO  
import Control.Monad
import GHC.Read (list)


countIncrease :: [Int] -> Int 
countIncrease (h1:(h2:(h3:(h4:t))))
  | h1+h2+h3 < h2+h3+h4 = 1 + countIncrease (h2:(h3:(h4:t)))
  | otherwise = countIncrease (h2:(h3:(h4:t)))
countIncrease _ = 0

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