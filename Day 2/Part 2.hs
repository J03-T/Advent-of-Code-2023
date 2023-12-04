import System.IO
import Data.List (isSuffixOf)

splitOn :: Char -> String -> [String]
splitOn c [] = []
splitOn c s = fst (break ( == c ) s):splitOn c (drop 1 (snd (break ( == c) s)))

gameId :: String -> Integer
gameId s = do
    let prefix = head (splitOn ':' s)
    let id = last (splitOn ' ' prefix)
    read id :: Integer

splitColours :: String -> [String]
splitColours s = do
    let gameSection = last (splitOn ':' s)
    let games = splitOn ';' gameSection
    concatMap (fmap tail .splitOn ',' ) games

maxOfColour :: String -> [String] -> Integer
maxOfColour c s = maximum (fmap (\x -> read (head (splitOn ' ' x)) :: Integer) (filter (isSuffixOf c) s))

minPower :: String -> Integer
minPower s = 
    let
        colours = splitColours s
        red = maxOfColour "red" colours
        blue = maxOfColour "blue" colours
        green = maxOfColour "green" colours
    in red * blue * green

main :: IO() = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    print ((sum . fmap minPower) (lines contents))
    hClose handle