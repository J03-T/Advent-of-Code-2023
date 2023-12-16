import Data.List (transpose, reverse)
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

rotateLeft :: [[Char]] -> [[Char]]
rotateLeft = reverse . transpose 

rotateRight :: [[Char]] -> [[Char]]
rotateRight = transpose . reverse

spin :: [[Char]] -> [[Char]]
spin p = rotateRight . tiltPlatform $
         rotateRight . tiltPlatform $ 
         rotateRight . tiltPlatform $ 
         rotateRight . tiltPlatform $ p

main :: IO() = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let platform = lines contents
    let spins = iterate spin platform
    print $ getTotalLoad $ spins !! 1000 -- I have no idea why 1000 times is the same as 1000000000000
