import System.IO
import Data.Char (isUpper, digitToInt)
import Data.Foldable (maximumBy)
import Data.Ord

-- Based on formula that x1 is a lower bound of natural sum, while x2 is the upper bound
generateXCoords :: (Int, Int) -> [Int]
generateXCoords (x1, x2) = [(floor ((3*(sqrt x11)-1) / 2))..(floor ((3*(sqrt x22)-1) / 2))]
    where x11 :: Float
          x11 = fromIntegral x1
          x22 = fromIntegral x2


addIfNotNull :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
addIfNotNull l (x,y) 
    | x == 0 && y == 0 = l
    | otherwise = (x,y):l

shoot :: (Int, Int, Int, Int) -> (Int, Int) -> (Int, Int, Int, Int) -> (Int, Int)
shoot (x,y,xVel,yVel) initialVelocity (bX1,bX2,bY1,bY2)
    | inArea = initialVelocity
    | y < bY1 || x > bX2 = (0,0)
    | otherwise = shoot (x+xVel,y+yVel,newVelX,yVel-1) initialVelocity (bX1,bX2,bY1,bY2)
    where inArea = x >= bX1 && x <= bX2 && y >= bY1 && y <= bY2
          newVelX
              | xVel > 0 = xVel - 1
              | xVel < 0 = xVel + 1
              | otherwise = 0

simulate :: [Int] -> [Int] -> (Int, Int, Int, Int) -> [(Int, Int)]
simulate xVels yVels border = foldl (\acc1 xVel -> acc1++(foldl (\acc2 yVel -> addIfNotNull acc2 (shoot (0,0,xVel,yVel) (xVel,yVel) border)) [] yVels)) [] xVels

main = do
        let list = []
        handle <- openFile "input_test.txt" ReadMode
        contents <- hGetContents handle
        print contents
        let parsedInput = parseInput contents
        let x = fst parsedInput
        let y = snd parsedInput
        let border = (fst x, snd x, fst y, snd y)

        print x
        print y

        let xCoords = [0..snd x] -- generateXCoords x
        let yCoords = [fst y..500]

        print $ generateXCoords x

        let sim = simulate xCoords yCoords border
        print border
        print sim
        print $ length sim
        --print $ maximumBy (comparing fst) (map (\(height,x,y) -> (height, (x,y))) filteredSim)

        print "qwe"

parseInput :: String -> ((Int,Int), (Int, Int))
parseInput s =
    let s1 = split '=' s
        s2 = split ',' (s1!!1)
        s3 = split '.' (s2!!0)
        x1 = read (s3!!0)
        x2 = read (s3!!2)
        s4 = split '.' (s1!!2)
        y1 = read (s4!!0)
        y2 = read (s4!!2)
    in ((x1,x2), (y1,y2))

-- From stackoverflow
split :: Char -> String -> [String]
split _ "" = []
split c s = firstWord : (split c rest)
    where firstWord = takeWhile (/=c) s
          rest = drop (length firstWord + 1) s