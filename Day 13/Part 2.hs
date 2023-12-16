import System.IO
import Data.List (transpose, zip)
import Debug.Trace (trace)

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

stringDiffCount :: String -> String -> Integer
stringDiffCount s1 s2 = fromIntegral $ length [(a, b) | (a, b) <- zip s1 s2, a /= b]

checkSmudgedReflects :: [String] -> Integer -> Integer -> Bool
checkSmudgedReflects p c d = not (c + d + 1 >= fromIntegral (length p) || c - d < 0) &&
    let
        bot = p!!fromIntegral (c - d)
        top = p!!fromIntegral (c + d + 1)
    in case stringDiffCount bot top of {
        0 -> checkSmudgedReflects p c (d + 1);
        1 -> checkReflects p c (d + 1);
        n -> False
    }

getReflectionRows :: [String] -> Integer
getReflectionRows p = let n = head ((filter (\x -> checkReflects p x 0) [0..fromIntegral (length p - 1)])++[-1]) + 1 in
    if n < fromIntegral (length p) then n else 0

getSmudgeRows :: [String] -> Integer
getSmudgeRows p = let n = head ((filter (\x -> checkSmudgedReflects p x 0) [0..fromIntegral (length p - 1)])++[-1]) + 1 in
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
    let sumOfColumns = sum $ map getSmudgeRows transpatterns
    print (map getSmudgeRows transpatterns)
    print sumOfColumns
    let sumOfRows = sum $ map getSmudgeRows patterns
    print sumOfRows
    print $ sumOfRows * 100 + sumOfColumns
