module Structures where

import Graphics.UI.Gtk

import Data.Char
import Control.Concurrent
import Control.Monad
--import System.IO

import InputOutput
import CheckFunctions
import Defines

addComp2Table :: WidgetClass a => Table -> a -> Int -> Int -> Int -> Int -> IO ()
addComp2Table table widg lAtt rAtt tAtt bAtt = 
    do 
        tableAttachDefaults table widg lAtt rAtt tAtt bAtt

addUnitComp2Table :: WidgetClass a => Table -> a -> Int -> Int -> IO ()
addUnitComp2Table table widg row col = 
    addComp2Table table widg row (row + 1) col (col + 1) 

getButtonFromField :: ButtonField -> Int -> Int -> IO RowColButton
getButtonFromField field row col = (field !! (row-1)) !! (col-1)

--createNewButton :: String -> IO Button
--createNewButton str = 
--    do 
--        button <- buttonNewWithLabel str
--        widgetModifyBg button StateNormal (Color 50000 50000 50000)
--        return button

createNewRowColButton :: Int -> Int -> String -> Correctness -> Solution -> IO RowColButton
createNewRowColButton i j string correctness solution = 
    do
        button <- buttonNewWithLabel string
        widgetModifyBg button StateNormal (Color 50000 50000 50000)
        let rowColButton = (RowColButton button i j)
        onClicked button (buttonFunction rowColButton correctness solution)
        return (rowColButton)

createButtonField :: Int -> Int -> Correctness -> Solution -> ButtonField
createButtonField rows cols correctness solution =
    [[createNewRowColButton i j " " correctness solution | i <-[0..(rows-1)]] | j <- [0..(cols-1)]]

halfInt :: Int -> Int
halfInt n = n `div` 2 + 1 

createTable :: Int -> Int -> IO Table
createTable rows cols =
    tableNew (rows + (halfInt rows)) (cols + (halfInt cols)) True

setField2TableCol :: Table -> [IO RowColButton] -> Int -> Int -> IO ()
setField2TableCol _ [] _ _ = return ()
setField2TableCol table (rowColButton:buttons) i j= 
    do
        (RowColButton button row col) <- rowColButton
        addUnitComp2Table table button i j
        setField2TableCol table buttons i (j+1)

setField2TableRow :: Table -> ButtonField -> Int -> Int -> IO ()
setField2TableRow _ [] _ _ = return ()
setField2TableRow table (part:field) i j = do
    setField2TableCol table part i j 
    setField2TableRow table field (i+1) j

--retirar
setField2Table :: Table -> ButtonField -> Int -> Int -> IO ()
setField2Table table field rows cols= setField2TableRow table field (halfInt rows) (halfInt cols)

        
setRowCol :: Table -> [IO Label] -> Int -> Int -> Bool -> IO ()
setRowCol _ [] _ _ _ = return ()
setRowCol table (labIO:labels) row col isRow = 
    do 
        lab <- labIO
        addUnitComp2Table table lab col row  
        setRowCol table labels newRow newCol isRow
            where 
                newRow = row + (fromEnum (not isRow))
                newCol = col + (fromEnum isRow)

setInfoRow2Table :: Table -> InfoRows -> Int -> Int -> IO ()
setInfoRow2Table _ [] _ _ = return ()
setInfoRow2Table table (iRow:infoRows) row col = 
    do 
        setRowCol table iRow row (col-(length iRow)) True
        setInfoRow2Table table infoRows (row + 1) col

--rever con setInfoCol2Table
setInfoCol2Table :: Table -> InfoCols -> Int -> Int -> IO ()
setInfoCol2Table _ [] _ _ = return ()
setInfoCol2Table table (iCol:infoCols) row col = 
    do 
        setRowCol table iCol (row-(length iCol)) col False
        setInfoCol2Table table infoCols row (col + 1)

setInfos2Table :: Table -> InfoRows -> InfoCols -> Int -> Int -> IO ()
setInfos2Table table infoRows infoCols rowMax colMax = 
    do
        setInfoRow2Table table infoRows rowMax colMax 
        setInfoCol2Table table infoCols rowMax colMax 

createCorrectnees :: Int -> Int -> IO Correctness
createCorrectnees nRows nCols =
    do
        let matrix = [ [newMVar False | i <- [0..(nRows-1)] ] | j <- [0..(nCols - 1)] ]  
        sGame <- newEmptyMVar
        putMVar sGame False
        mvarMatrix <- newMVar matrix
        return ((Correctness mvarMatrix sGame))

        

createFullTable :: FilePath -> IO FullTable 
createFullTable phase = 
    do 
        let path = "LevelsFiles/" ++ phase ++ "/" ++ phase
        let ext = ".haskkross"
        solution <- readNProcessFile (path ++ "Solution" ++ ext )
        rowsRead <- readNProcessFile (path ++ "Rows" ++ ext)
        colsRead <- readNProcessFile (path ++ "Cols" ++ ext)
        correctness <- createCorrectnees 5 5
        let rows = length solution
        let cols = length (solution !! 0)
        let field = createButtonField rows cols correctness solution 
        let infoRows = createInfoList rowsRead
        let infoCols = createInfoList colsRead
        table <- createTable rows cols
        setField2Table table field rows cols
        setInfos2Table table infoRows infoCols (halfInt rows) (halfInt cols)
        --enableFieldFunction field
        return (FullTable table field infoRows infoCols solution correctness)

createInfoList :: [[Int]] -> [[IO Label]]
createInfoList matrix = map labelList matrix
    where
        labelList :: [Int] -> [IO Label]
        labelList list = map (\s -> labelNew (Just (show s))) list

getButtonFromTable :: TableField -> Int -> Int -> IO RowColButton
getButtonFromTable (TableField table field) row col = 
    getButtonFromField field row col

buttonFunction2 :: Button -> IO ()
buttonFunction2 button = 
    do
        txt <- buttonGetLabel button
        let newTxt = case txt of
                        " " -> "  "
                        "  " -> "X"
                        "X" -> " "
        buttonSetLabel button newTxt
        
buttonFunction :: RowColButton -> Correctness -> Solution -> IO ()
buttonFunction (RowColButton button i j) correctness solution =  
    do
        txt <- buttonGetLabel button
        newTxt <-   if txt == " " 
                    then (do widgetModifyBg button StateNormal (Color 0 0 0); return "  ") 
                    else if txt == "  " 
                        then (do widgetModifyBg button StateNormal (Color 65535 65535 65535); return "X")
                        else (do widgetModifyBg button StateNormal (Color 50000 50000 50000); return " ")
        buttonSetLabel button newTxt
        checkEndGame correctness (RowColButton button i j) solution 
        let (Correctness matrix endgame) = correctness
        bool <- readMVar endgame
        --print(bool)
        if bool then stopGame else return ()

stopGame :: IO ()
stopGame = do
    initGUI
    window <- windowNew
    print("a")
    setWindowProps2 "Step One" window

    onDestroy window mainQuit
    widgetShowAll window
    mainGUI

setWindowProps2 :: String -> Window -> IO ()
setWindowProps2 title window = 
    set window [windowTitle := title, containerBorderWidth := 20,
                windowDefaultWidth := 500, windowDefaultHeight := 500]