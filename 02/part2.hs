import System.IO  
import Control.Monad
import GHC.Read (list)


moveSubmarine :: [(String, Int)] -> (Int, Int, Int) -> Int 
moveSubmarine [] (pos,depth,aim) = pos*depth
moveSubmarine ((dir, x):t) (pos,depth,aim)
  | dir == "forward" = moveSubmarine t (pos+x,depth+x*aim,aim)
  | dir == "up" = moveSubmarine t (pos,depth,aim-x)
  | otherwise = moveSubmarine t (pos,depth,aim+x)


main = do  
        let list = []
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
            list = inputToTuples singlewords
        print list
        print (moveSubmarine list (0,0,0))

inputToTuples :: [String] -> [(String, Int)]
inputToTuples (h1:h2:t) = (h1,read h2): inputToTuples t
inputToTuples _ = []