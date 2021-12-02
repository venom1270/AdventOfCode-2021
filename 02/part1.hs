import System.IO  
import Control.Monad
import GHC.Read (list)


moveSubmarine :: [(String, Int)] -> (Int, Int) -> Int 
moveSubmarine [] (x,y) = x * y
moveSubmarine ((dir, val):t) (x,y)
  | dir == "forward" = moveSubmarine t (x+val, y)
  | dir == "up" = moveSubmarine t (x, y-val)
  | otherwise = moveSubmarine t (x, y+val)


main = do  
        let list = []
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
            list = inputToTuples singlewords
        print list
        print (moveSubmarine list (0,0))

inputToTuples :: [String] -> [(String, Int)]
inputToTuples (h1:h2:t) = (h1,read h2): inputToTuples t
inputToTuples _ = []