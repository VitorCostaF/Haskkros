module InputOutput where

import Graphics.UI.Gtk

dropWhileMore1 :: Char -> String -> String
dropWhileMore1 _ [] = []
dropWhileMore1 char string = drop 1 (dropWhile ( /= char) string)

splitLines :: Char -> String -> [String]
splitLines _ [] = []
splitLines char string = (takeWhile (/=char) string) : (splitLines char (dropWhileMore1 char string))


removeSeparator :: Char -> String -> String
removeSeparator _ [] = []
removeSeparator separator string = 
    (takeWhile (/= separator) string) ++ (removeSeparator separator (dropWhileMore1 separator string))
    
    
removeSeparatorEx :: Char -> [String] -> [String]
removeSeparatorEx _ [] = []
removeSeparatorEx separator (strPart:string) = 
    (removeSeparator separator strPart):(removeSeparatorEx separator string)

--splitNumbers :: String -> [Int]
--splitNumbers char (strPart:string) = 
--
--processeFile :: String -> [[Int]]
--processeFile content = removeSeparatorEx $ splitLines '\n' content

{-
readNProcessFile :: String -> IO [[Int]]
readNProcessFile fileName = 
    do 
        file <- readFile fileName
        let content = processeFile file
        return content
-}