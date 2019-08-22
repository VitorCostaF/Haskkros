module Structures where

import Graphics.UI.Gtk

import Data.Char
import Control.Concurrent
import Control.Monad
--import System.IO

<<<<<<< HEAD
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
=======
type ButtonField = [[IO RowColButton]]
type InfoRows = [[IO Label]]
type InfoCols = [[IO Label]]
type Solution = [[Int]]

data RowColButton = RowColButton (IO Button) Int Int

data TableField = TableField Table ButtonField

data FullTable = FullTable Table ButtonField InfoRows InfoCols Solution

addComp2Table :: WidgetClass a => Table ->IO a -> Int -> Int -> Int -> Int -> IO ()
addComp2Table table widg lAtt rAtt tAtt bAtt =
    do
        aux <- widg
        tableAttachDefaults table aux lAtt rAtt tAtt bAtt

addUnitComp2Table :: WidgetClass a => Table -> IO a -> Int -> Int -> IO ()
addUnitComp2Table table widg row col =
    addComp2Table table widg row (row + 1) col (col + 1)
>>>>>>> 9baa13c1c7514ca362f16733f531c5b039ec1a00

getButtonFromField :: ButtonField -> Int -> Int -> IO RowColButton
getButtonFromField field row col = (field !! (row-1)) !! (col-1)

<<<<<<< HEAD
--createNewButton :: String -> IO Button
--createNewButton str = 
--    do 
--        button <- buttonNewWithLabel str
--        widgetModifyBg button StateNormal (Color 50000 50000 50000)
--        return button

createNewRowColButton :: Int -> Int -> String -> Correctness -> Solution -> IO RowColButton
createNewRowColButton i j string correctness solution = 
=======
createNewButton :: String -> IO Button
createNewButton str =
    do
        button <- buttonNewWithLabel str
        widgetModifyBg button StateNormal (Color 50000 50000 50000)
        buttonSetFocusOnClick button False
        onClicked button (buttonFunction button)
        return button

createNewRowColButton :: Int -> Int -> String -> IO RowColButton
createNewRowColButton i j string =
>>>>>>> 9baa13c1c7514ca362f16733f531c5b039ec1a00
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
<<<<<<< HEAD
setRowCol table (labIO:labels) row col isRow = 
    do 
        lab <- labIO
        addUnitComp2Table table lab col row  
=======
setRowCol table (lab:labels) row col isRow =
    do
        addUnitComp2Table table lab col row
>>>>>>> 9baa13c1c7514ca362f16733f531c5b039ec1a00
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
{-
createFullTable :: FilePath -> IO FullTable
createFullTable phase =
    do
<<<<<<< HEAD
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
=======
>>>>>>> 9baa13c1c7514ca362f16733f531c5b039ec1a00
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
<<<<<<< HEAD
        return (FullTable table field infoRows infoCols solution correctness)

createInfoList :: [[Int]] -> [[IO Label]]
createInfoList matrix = map labelList matrix
    where
        labelList :: [Int] -> [IO Label]
        labelList list = map (\s -> labelNew (Just (show s))) list
=======
        return (FullTable table field infoRows infoCols solution)
-}

-- Alterado:
createFullTable :: FilePath -> IO FullTable
createFullTable phase =
    do
        let path = "LevelsFiles/" ++ phase ++ "/" ++ phase
        let ext = ".haskkross"
        solution <- readNProcessFile (path ++ "Solution" ++ ext )
        --rowsRead <- readNProcessFile (path ++ "Rows" ++ ext)
        --colsRead <- readNProcessFile (path ++ "Cols" ++ ext)
        let rows = length solution
        let cols = length (solution !! 0)
        let field = createButtonField rows cols
        let infoRows = createInfoListRow solution
        print(length (infoRows!!0) )
        let infoCols = createInfoListCol solution
        --let solution = createSolution rows cols
        table <- createTable rows cols
        setField2Table table field rows cols
        setInfos2Table table infoRows infoCols (halfInt rows) (halfInt cols)
        --enableFieldFunction field
        return (FullTable table field infoRows infoCols solution)

--createInfoList :: [[Int]] -> [[IO Label]]
--createInfoList matrix = map labelList matrix

--Alterado
createInfoListRow :: [[Int]] -> [[IO Label]]
createInfoListRow matrix = map labelList $ reverse $ [createLabel z [0] (-1) | z <- (reverse matrix)]
--Alterado
createInfoListCol :: [[Int]] -> [[IO Label]]
createInfoListCol matrix = map labelList $ reverse [createLabel (map (!! n) (reverse matrix)) [0] (-1) | n <- [0..((length matrix)-1 )]]
--Novo
createLabel :: [Int] -> [Int] -> Int -> [Int]
createLabel [] c _ = init c
createLabel (x:xs) (c:cs) y
  | x == 0    = createLabel xs (c:cs) (-1)
  | x == y    = createLabel xs ((c+1):cs) (x)
  | otherwise = createLabel xs (1:c:cs) (x)


labelList :: [Int] -> [IO Label]
labelList list = map (\s -> labelNew (Just (show s))) list
>>>>>>> 9baa13c1c7514ca362f16733f531c5b039ec1a00

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
<<<<<<< HEAD
        
buttonFunction :: RowColButton -> Correctness -> Solution -> IO ()
buttonFunction (RowColButton button i j) correctness solution =  
=======

buttonFunction :: Button -> IO ()
buttonFunction button =
>>>>>>> 9baa13c1c7514ca362f16733f531c5b039ec1a00
    do
        txt <- buttonGetLabel button
        newTxt <-   if txt == " "
                    then (do widgetModifyBg button StateNormal (Color 0 0 0); return "  ")
                    else if txt == "  "
                        then (do widgetModifyBg button StateNormal (Color 65535 65535 65535); return "X")
                        else (do widgetModifyBg button StateNormal (Color 50000 50000 50000); return " ")
        buttonSetLabel button newTxt
<<<<<<< HEAD
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
=======
>>>>>>> 9baa13c1c7514ca362f16733f531c5b039ec1a00
