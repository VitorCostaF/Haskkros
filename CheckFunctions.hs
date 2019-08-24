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


createNewMatrix :: MatrixMVar -> Int -> Int -> [Bool] -> MatrixMVar
createNewMatrix [] _ _ _ = []
createNewMatrix (m:matrix) line it newLine 
    | it == line = newLine : matrix
    | otherwise = m : createNewMatrix matrix line (it+1) newLine

newState :: MVar MatrixMVar -> Int -> Int -> Button -> [[Int]] -> IO ()
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
checkRow [] = True
checkRow (elem:row) = elem && (checkRow row)


checkMatrix :: MatrixMVar -> Bool
checkMatrix [] = True
checkMatrix (row:matrix) = (checkRow row) && (checkMatrix matrix)

checkEndGame :: Correctness -> RowColButton -> Solution -> IO ()
checkEndGame (Correctness mvarMatrix endGame) (RowColButton button i j) solution =
    do
        vEndGame <- takeMVar endGame
        newState mvarMatrix i j button solution
        matrix <- readMVar mvarMatrix
        let newEndGame = checkMatrix matrix 
        putMVar endGame newEndGame
        return ()



--funcoes para testes apenas
printaSolLine :: [Int] -> String -> IO ()
printaSolLine [] acc = do print ( acc)
printaSolLine (elem:line) acc = 
    do 
        printaSolLine line (acc ++ (show elem) ++ " ") 

printaSol :: Solution -> IO ()
printaSol [] = do print("================================")
printaSol (line:lines) =
    do 
        printaSolLine line " "
        printaSol lines


printaLine :: [Bool] -> String -> IO ()
printaLine [] acc = do print ( acc)
printaLine (elem:line) acc = 
    do 
        printaLine line (acc ++ (show elem) ++ " ") 

printaMatrix :: MatrixMVar -> IO ()
printaMatrix [] = do print("================================")
printaMatrix (line:lines) =
    do 
        printaLine line " "
        printaMatrix lines
