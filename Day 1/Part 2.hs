import Data.Char (isDigit)
import System.IO

wordsToDigits :: [Char] -> [Char]
wordsToDigits [] = []
wordsToDigits a = case a of {
    -- last character of word is added to make cases like "twone" parse properly ("21")
    ('o':'n':'e':s) -> '1':wordsToDigits ('e':s);
    ('t':'w':'o':s) -> '2':wordsToDigits ('o':s);
    ('t':'h':'r':'e':'e':s) -> '3':wordsToDigits ('e':s);
    ('f':'o':'u':'r':s) -> '4':wordsToDigits ('r':s);
    ('f':'i':'v':'e':s) -> '5':wordsToDigits ('e':s);
    ('s':'i':'x':s) -> '6':wordsToDigits ('x':s);
    ('s':'e':'v':'e':'n':s) -> '7':wordsToDigits ('n':s);
    ('e':'i':'g':'h':'t':s) -> '8':wordsToDigits ('t':s);
    ('n':'i':'n':'e':s) -> '9':wordsToDigits ('e':s);
    (x:xs) -> x: wordsToDigits xs
}

firstDigit :: [Char] -> Char
firstDigit (x:xs) = if isDigit x then x else firstDigit xs

lastDigit :: [Char] -> Char
lastDigit a = firstDigit (reverse a)

calibrationValue :: [Char] -> Integer
calibrationValue a = read (firstDigit (wordsToDigits a) : [lastDigit (wordsToDigits a)]) :: Integer

main :: IO() = do
    handle <- openFile "input.txt" ReadMode
    fileContents <- hGetContents handle
    let fileLines = lines fileContents
    let calValues = map calibrationValue fileLines
    print calValues
    let total = foldr (+) (head calValues) (tail calValues)
    print total
    hClose handle