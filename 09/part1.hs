import System.IO
import Data.Char (digitToInt)


solveBad :: [[Int]] -> Int -> Int -> [Int]
solveBad l x y 
    | x == n && y == m && l!!(x-1)!!y > el && l!!x!!(y-1) > el = [el]
    | x == n && y == m = []
    | y == m && x == 0 && l!!x!!(y-1) > el && l!!(x+1)!!y > el = el : solveBad l (x+1) y
    | y == m && x == 0 = solveBad l (x+1) y
    | y == m && l!!(x-1)!!y > el && l!!x!!(y-1) > el && l!!(x+1)!!y > el = el : solveBad l (x+1) y
    | y == m = solveBad l (x+1) y
    | x == n && y == 0 && l!!(x-1)!!y > el && l!!x!!(y+1) > el = el : solveBad l 0 (y+1)
    | x == n && y == 0 = solveBad l (x+1) y
    | x == n && y /= 0 && l!!(x-1)!!y > el && l!!x!!(y+1) > el && l!!x!!(y-1) > el = el : solveBad l 0 (y+1)
    | x == n && y /= 0 = solveBad l (x+1) y
    | nextRow = solveBad l 0 (y+1)
    | x == 0 && y == 0 && l!!(x+1)!!y > el && l!!x!!(y+1) > el = el : solveBad l (x+1) y
    | x == 0 && y == 0 = solveBad l (x+1) y
    | x == 0 && y /= 0 && l!!(x+1)!!y > el && l!!x!!(y+1) > el && l!!x!!(y-1) > el = el : solveBad l (x+1) y
    | y == 0 && x/= 0 && l!!(x+1)!!y > el && l!!x!!(y+1) > el && l!!(x-1)!!y > el = el : solveBad l (x+1) y
    | x > 0 && x < n && y > 0 && y < m && l!!x!!(y-1) > el && l!!(x+1)!!y > el && l!!x!!(y+1) > el && l!!(x-1)!!y > el = el : solveBad l (x+1) y
    | otherwise = solveBad l (x+1) y
    where n = length l - 1
          m = length (head l) - 1
          el = l!!minimum [x,n]!!minimum [y,m]
          nextRow = x >= n

getSolution :: [Int] -> Int
getSolution s = sum (map (+1) s)

main = do
        let list = []
        handle <- openFile "input_test.txt" ReadMode
        contents <- hGetContents handle
        --print (lines contents)
        let parsedInput = parseInput (lines contents)
        print parsedInput
        --print ((parsedInput!!4)!!4)
        print (getSolution $ solveBad parsedInput 0 0)



parseInput :: [String] -> [[Int]]
parseInput = map $ map digitToInt
