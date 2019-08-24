module Structures where

import Graphics.UI.Gtk

import Data.Char
import Control.Concurrent
import Control.Monad

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

<<<<<<< HEAD

createNewRowColButton :: Int -> Int -> String -> Correctness -> Solution -> Table -> Image -> IO RowColButton
createNewRowColButton i j string correctness solution table image = 
=======
--createNewButton :: String -> IO Button
--createNewButton str =
--    do
--        button <- buttonNewWithLabel str
--        widgetModifyBg button StateNormal (Color 50000 50000 50000)
--        return button

createNewRowColButton :: Int -> Int -> String -> Correctness -> Solution -> IO RowColButton
createNewRowColButton i j string correctness solution =
>>>>>>> c10657815250bca194bdab616aed8ed5356bafbb
    do
        button <- buttonNewWithLabel string
        widgetModifyBg button StateNormal (Color 50000 50000 50000)
        let rowColButton = (RowColButton button i j)
        onClicked button (buttonFunction rowColButton correctness solution table image)
        return (rowColButton)
<<<<<<< HEAD
        
createButtonField :: Int -> Int -> Correctness -> Solution -> Table -> Image -> ButtonField
createButtonField rows cols correctness solution table image =
    [[createNewRowColButton i j " " correctness solution table image | i <-[0..(rows-1)]] | j <- [0..(cols-1)]]
=======

createButtonField :: Int -> Int -> Correctness -> Solution -> ButtonField
createButtonField rows cols correctness solution =
    [[createNewRowColButton i j " " correctness solution | i <-[0..(rows-1)]] | j <- [0..(cols-1)]]
>>>>>>> c10657815250bca194bdab616aed8ed5356bafbb

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
<<<<<<< HEAD

=======
{-}
createFullTable :: FilePath -> IO FullTable
createFullTable level =
    do
        setInfoRow2Table table infoRows rowMax colMax
        setInfoCol2Table table infoCols rowMax colMax
-}
>>>>>>> c10657815250bca194bdab616aed8ed5356bafbb
createCorrectnees :: Int -> Int -> IO Correctness
createCorrectnees nRows nCols =
    do
        let matrix = [ [ False | i <- [0..(nRows-1)] ] | j <- [0..(nCols - 1)] ]
        sGame <- newEmptyMVar
        putMVar sGame False
        mvarMatrix <- newMVar matrix
        return ((Correctness mvarMatrix sGame))

<<<<<<< HEAD
=======

{-
createFullTable :: FilePath -> IO FullTable
createFullTable level =
    do
        let path = "LevelsFiles/" ++ level ++ "/" ++ level
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
        return (FullTable table field infoRows infoCols solution)
-}

>>>>>>> c10657815250bca194bdab616aed8ed5356bafbb
-- Alterado:
createFullTable :: FilePath -> IO FullTable
createFullTable level =
    do
        let path = "LevelsFiles/" ++ level ++ "/" ++ level
        let ext = ".haskross"
        solution <- readNProcessFile (path ++ ext )
        let rows = length solution
        let cols = length (solution !! 0)
        correctness <- createCorrectnees rows cols
        table <- createTable rows cols
        image <- imageNewFromFile "fimDeJogo.png" 
        let field = createButtonField rows cols correctness solution table image
        let infoRows = createInfoListRow solution
        let infoCols = createInfoListCol solution
        setField2Table table field rows cols
        setInfos2Table table infoRows infoCols (halfInt rows) (halfInt cols)
        return (FullTable table field infoRows infoCols solution correctness image)

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

getButtonFromTable :: TableField -> Int -> Int -> IO RowColButton
getButtonFromTable (TableField table field) row col =
    getButtonFromField field row col

<<<<<<< HEAD
        
buttonFunction :: RowColButton -> Correctness -> Solution -> Table -> Image -> IO ()
buttonFunction (RowColButton button i j) correctness solution table image =  
    do
=======

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
>>>>>>> c10657815250bca194bdab616aed8ed5356bafbb
        let (Correctness matrix endgame) = correctness
        bool <- readMVar endgame
        if bool then return ()
        else (do 
            txt <- buttonGetLabel button
            newTxt <-   if txt == " "
                        then (do widgetModifyBg button StateNormal (Color 0 0 0); return "  ")
                        else if txt == "  "
                            then (do widgetModifyBg button StateNormal (Color 65535 65535 65535); return "X")
                            else (do widgetModifyBg button StateNormal (Color 50000 50000 50000); return " ")
            buttonSetLabel button newTxt
            checkEndGame correctness (RowColButton button i j) solution 
            let (Correctness matrix endgame) = correctness
            newBool <- readMVar endgame
            --print(bool)
            if newBool then stopGame table image else return () )

stopGame :: Table -> Image ->  IO ()
stopGame table image = 
    do 
        addComp2Table table image 0 3 0 3 
        widgetShow image



setWindowProps2 :: String -> Window -> IO ()
setWindowProps2 title window =
    set window [windowTitle := title, containerBorderWidth := 20,
                windowDefaultWidth := 500, windowDefaultHeight := 500]
