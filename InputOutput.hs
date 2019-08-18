module InputOutput where

import Graphics.UI.Gtk
import Data.Char

stringToInt :: [Char] -> Int
stringToInt str = stringToInt' str (length str - 1)
    where 
        stringToInt' :: [Char] -> Int -> Int
        stringToInt' [] _ = 0
        stringToInt' (c:str) tam = ((digitToInt c)*(10^tam)) + (stringToInt' str (tam-1))

strToInt :: String -> Int
strToInt str = read str :: Int

splitLines :: Char -> String -> [String]
splitLines _ [] = []
splitLines char string = line : (splitLines char (drop ( (length line) + 1) string))
    where 
        line = takeWhile (/=char) string

splitNumbers :: Char -> String -> [Int]
splitNumbers _ [] = []
splitNumbers separator string =  
    (strToInt number) : (splitNumbers separator (drop (length number + 1) string))
    where 
        number = takeWhile (/= separator) string

splitNumbersEx :: Char -> [String] -> [[Int]]
splitNumbersEx separator strings = [splitNumbers separator string | string <- strings]

processeFile :: String -> [[Int]]
processeFile content = splitNumbersEx ' ' $ splitLines '\n' content


readNProcessFile :: FilePath -> IO [[Int]]
readNProcessFile fileName = 
    do 
        file <- readFile fileName
        let content = processeFile file
        return content
