module InputOutput where

import Graphics.UI.Gtk
import Data.Char

stringToInt :: [Char] -> Int
stringToInt str = stringToInt' str (length str - 1)
    where 
        stringToInt' :: [Char] -> Int -> Int
        stringToInt' [] _ = 0
        stringToInt' (c:str) tam = ((digitToInt c)*(10^tam)) + (stringToInt' str (tam-1))

dropWhileMore1 :: Char -> String -> String
dropWhileMore1 _ [] = []
dropWhileMore1 char string = drop 1 (dropWhile ( /= char) string)

splitLines :: Char -> String -> [String]
splitLines _ [] = []
splitLines char string = (takeWhile (/=char) string) : (splitLines char (dropWhileMore1 char string))


--removeSeparator :: Char -> String -> String
--removeSeparator _ [] = []
--removeSeparator separator string = 
--    (takeWhile (/= separator) string) ++ (removeSeparator separator (dropWhileMore1 separator string))
    
    
--removeSeparatorEx :: Char -> [String] -> [String]
--removeSeparatorEx _ [] = []
--removeSeparatorEx separator strings = [removeSeparator separator string | string <- strings]

splitNumbers :: Char -> String -> [Int]
splitNumbers _ [] = []
splitNumbers separator string =  
    (stringToInt (takeWhile (/= separator) string)) : (splitNumbers separator (dropWhileMore1 separator string))

splitNumbersEx :: Char -> [String] -> [[Int]]
splitNumbersEx separator strings = [splitNumbers separator string | string <- strings]

processeFile :: String -> [[Int]]
processeFile content = splitNumbersEx ' ' $ splitLines '\n' content


readNProcessFile :: FilePath -> IO [[Int]]
readNProcessFile fileName = 
    do 
        print(fileName)
        file <- readFile fileName
        let content = processeFile file
        return content
