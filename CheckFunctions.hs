module CheckFunctions where

import Graphics.UI.Gtk
import Control.Concurrent
import Control.Monad

import Defines
{-
type Solution = [[Int]]

type MVarBool = MVar Bool
type MatrixMVar = [[IO MVarBool]]
data RowColButton = RowColButton (IO Button) Int Int 
data Correctness = Correctness MatrixMVar (MVar Bool)
-}
buttonEqualSol :: String -> Int -> Bool
buttonEqualSol txt value 
    | (txt == "X") && (value == 0) = True
    | (txt == "  ") && (value == 1) = True
    | otherwise = False 


funcAux :: MVar Bool -> Bool -> IO (MVar Bool) 
funcAux var bool = 
    do 
        aux <- swapMVar var bool
        return var

newState :: MatrixMVar -> Int -> Int -> Button -> [[Int]] -> IO MatrixMVar
newState matrix i j button sol =
    do 
        let value = ((sol !! i) !! j)
        txt <- buttonGetLabel button
        --print(txt)
        val <- (matrix !! i !! j)
        let bool =  buttonEqualSol txt value
        x <- funcAux val bool
        return matrix
        


checkRow :: [IO (MVar Bool)] -> IO Bool
checkRow [] = return (True)
checkRow (elemIO:row) =
    do 
        elem <- elemIO
        vElem <- readMVar elem
        pure (&&) <*> (pure vElem) <*> (checkRow row)


checkMatrix :: MatrixMVar -> IO Bool
checkMatrix [] = do return(True)
checkMatrix (row:matrix) = 
    do 
        pure (&&) <*> (checkRow row) <*> (checkMatrix matrix)

checkEndGame :: Correctness -> RowColButton -> Solution -> IO ()
checkEndGame (Correctness mvarMatrix endGame) (RowColButton button i j) solution =
    do
        vEndGame <- takeMVar endGame
        matrix <- readMVar mvarMatrix
        newMatrix <- newState matrix i j button solution
        newEndGame <- checkMatrix matrix 
        x <- swapMVar mvarMatrix newMatrix
        --printaSol solution
        --printaMatrix matrix
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


printaLine :: [IO (MVar Bool)] -> String -> IO ()
printaLine [] acc = do print ( acc)
printaLine (elemIO:line) acc = 
    do 
        elem <- elemIO
        val <- readMVar elem
        printaLine line (acc ++ (show val) ++ " ") 

printaMatrix :: MatrixMVar -> IO ()
printaMatrix [] = do print("================================")
printaMatrix (line:lines) =
    do 
        printaLine line " "
        printaMatrix lines
