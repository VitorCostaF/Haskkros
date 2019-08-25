module InputOutput where

import Data.Char

strToInt :: String -> Int
strToInt str = read str :: Int

splitStr :: Char -> String -> [String]
splitStr _ [] = []
splitStr char string = line : (splitStr char (drop ((length line) + 1) string))
    where 
        line = takeWhile (/=char) string

strListToInt :: [String] -> [Int]
strListToInt string = map strToInt string

splitNumbers :: Char -> [String] -> [[Int]]
splitNumbers separator strings = map (\s -> strListToInt $ splitStr separator s) strings

processeFile :: String -> [[Int]]
processeFile content = splitNumbers ' ' $ splitStr '\n' content


readNProcessFile :: FilePath -> IO [[Int]]
readNProcessFile fileName = 
    do 
        file <- readFile fileName
        let content = processeFile file
        return content
