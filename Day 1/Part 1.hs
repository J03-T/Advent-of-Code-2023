import Data.Char
import System.IO

firstDigit :: [Char] -> Char
firstDigit (x:xs) = if isDigit x then x else firstDigit xs
lastDigit :: [Char] -> Char
lastDigit a = firstDigit (reverse a)

main :: IO() = do
    handle <- openFile "input.txt" ReadMode
    fileContents <- hGetContents handle
    let fileLines = lines fileContents
    let calibrationValues = map (\x -> read (firstDigit x : [lastDigit x]) :: Integer) fileLines
    let total = foldr (+) (head calibrationValues) (tail calibrationValues)
    print total
    hClose handle