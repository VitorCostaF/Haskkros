module CheckFunctions where

import Graphics.UI.Gtk
import Control.Concurrent
import Control.Monad

import Defines

buttonEqualSol :: String -> Int -> Bool
buttonEqualSol txt value 
    | (txt == "X") && (value == 0) = True
    | (txt == "  ") && (value == 1) = True
    | otherwise = False 

createNewLine :: [Bool] -> Int -> Int -> Bool -> [Bool]
createNewLine [] _ _ _ = []
createNewLine (elem:line) col it bool 
    | it == col = bool : line
    | otherwise = elem : createNewLine line col (it + 1) bool


createNewMatrix :: MatrixBool -> Int -> Int -> [Bool] -> MatrixBool
createNewMatrix [] _ _ _ = []
createNewMatrix (m:matrix) line it newLine 
    | it == line = newLine : matrix
    | otherwise = m : createNewMatrix matrix line (it+1) newLine

newState :: MVar MatrixBool -> Int -> Int -> Button -> [[Int]] -> IO ()
newState mvarMatrix i j button sol =
    do 
        let value = sol !! i !! j
        txt <- buttonGetLabel button
        matrix <- readMVar mvarMatrix
        let line = matrix !! i
        let newBool =  buttonEqualSol txt value
        let newLine = createNewLine line j 0 newBool 
        let newMatrix = createNewMatrix matrix i 0 newLine
        aux <- swapMVar mvarMatrix newMatrix
        return ()

checkRow :: [Bool] -> Bool
checkRow row = foldr (&&) True row

checkMatrix :: MatrixBool -> Bool
checkMatrix  matrix = foldr (\a b -> checkRow a && b) True matrix

checkEndGame :: Correctness -> RowColButton -> Solution -> IO ()
checkEndGame (Correctness mvarMatrix endGame) (RowColButton button i j) solution =
    do
        vEndGame <- takeMVar endGame
        newState mvarMatrix i j button solution
        matrix <- readMVar mvarMatrix
        let newEndGame = checkMatrix matrix 
        putMVar endGame newEndGame
        return ()



--funções para visualização da matriz de soluções
--usadas apenas para testes 
printaSolLine :: [Int] -> String -> String
printaSolLine line acc = foldr (\s a-> " " ++ (show s) ++ a) acc line

printaSol :: Solution -> IO ()
printaSol [] = do print("================================")
printaSol (line:lines) =
    do 
        print(printaSolLine line " ")
        printaSol lines

--funções para conferencia da matrix MVar 
--usadas apenas para testes 
printaLine :: [Bool] -> String -> String
printaLine line acc = foldr (\s a-> " " ++ (show s) ++ a) acc line

printaMatrix :: MatrixBool -> IO ()
printaMatrix [] = do print("================================")
printaMatrix (line:lines) =
    do 
        print(printaLine line " ")
        printaMatrix lines