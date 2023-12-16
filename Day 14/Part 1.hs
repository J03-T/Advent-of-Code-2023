import Data.List (transpose)
import System.IO

rollRowNorthOnce :: [Char] -> [Char]
rollRowNorthOnce [] = []
rollRowNorthOnce ('.':'O':xs) = 'O':'.':rollRowNorthOnce xs
rollRowNorthOnce (x:xs) = x:rollRowNorthOnce xs

needsSwap :: [Char] -> Bool
needsSwap [] = False
needsSwap ('.':'O':xs) = True
needsSwap (x:xs) = needsSwap xs

rollRowNorth :: [Char] -> [Char]
rollRowNorth [] = []
rollRowNorth x = if needsSwap x then rollRowNorth (rollRowNorthOnce x) else x

tiltPlatform :: [[Char]] -> [[Char]]
tiltPlatform p = transpose $ map rollRowNorth (transpose p)

countOfChar :: Char -> [Char] -> Integer
countOfChar c = fromIntegral . length . filter (==c)

getLineLoad :: [Char] -> Integer -> Integer
getLineLoad l i = i * countOfChar 'O' l

getTotalLoad :: [[Char]] -> Integer
getTotalLoad p = sum $ map (\i -> getLineLoad (p!!(length p - i)) (fromIntegral i)) [1..length p]

main :: IO() = do
    handle <- openFile "input.txt" ReadMode
    input <- hGetContents handle
    let platform = lines input
    let tiltedPlatform = tiltPlatform platform
    print (getTotalLoad tiltedPlatform)
