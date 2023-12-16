import System.IO
import Data.List (transpose)

splitOn :: String -> [String] -> [[String]]
splitOn d [] = []
splitOn d s = fst (break ( == d ) s):splitOn d (drop 1 (snd (break ( == d) s)))

splitPatterns :: [String] -> [[String]]
splitPatterns = splitOn ""

checkReflects :: [String] -> Integer -> Integer -> Bool
checkReflects p c d = (c + d + 1 >= fromIntegral (length p) || c - d < 0) ||
    let
        bot = p!!fromIntegral (c - d)
        top = p!!fromIntegral (c + d + 1)
    in (bot == top && checkReflects p c (d + 1))


getReflectionRows :: [String] -> Integer
getReflectionRows p = let n =  head (filter (\x -> checkReflects p x 0) [0..fromIntegral (length p - 1)]) + 1 in
    if n < fromIntegral (length p) then n else 0

parseInput :: String -> IO [[String]]
parseInput f = do
    handle <- openFile f ReadMode
    contents <- hGetContents handle
    let l = lines contents
    let p = splitPatterns l
    return p
    

main :: IO() = do
    patterns <- parseInput "input.txt"
    let transpatterns = map transpose patterns
    let sumOfColumns = sum $ map getReflectionRows transpatterns
    print sumOfColumns
    let sumOfRows = sum $ map getReflectionRows patterns
    print sumOfRows
    print $ sumOfRows * 100 + sumOfColumns
