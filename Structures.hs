module Structures where

import Graphics.UI.Gtk

import Data.Char
import Control.Concurrent
import Control.Monad

import InputOutput
import CheckFunctions
import Defines

addComp2Table :: WidgetClass a => Table -> a -> Int -> Int -> Int -> Int -> IO ()
addComp2Table table widg lAtt rAtt tAtt bAtt = tableAttachDefaults table widg lAtt rAtt tAtt bAtt

createNewRowColButton :: Int -> Int -> String -> Correctness -> Solution -> Table -> Image -> IO RowColButton
createNewRowColButton i j string correctness solution table image =
    do
        button <- buttonNewWithLabel string
        widgetModifyBg button StateNormal (Color 50000 50000 50000)
        let rowColButton = (RowColButton button i j)
        onClicked button (buttonFunction rowColButton correctness solution table image)
        return (rowColButton)

createButtonField :: Int -> Int -> Correctness -> Solution -> Table -> Image -> ButtonField
createButtonField rows cols correctness solution table image =
    [[createNewRowColButton i j " " correctness solution table image | i <-[0..(rows-1)]] | j <- [0..(cols-1)]]

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
        addComp2Table table button i (i+1) j (j+1)
        setField2TableCol table buttons i (j+1)

setField2TableRow :: Table -> ButtonField -> Int -> Int -> IO ()
setField2TableRow _ [] _ _ = return ()
setField2TableRow table (part:field) i j = do
    setField2TableCol table part i j
    setField2TableRow table field (i+1) j

setRowCol :: Table -> [IO Label] -> Int -> Int -> Bool -> IO ()
setRowCol _ [] _ _ _ = return ()
setRowCol table (labIO:labels) row col isRow =
    do
        lab <- labIO
        addComp2Table table lab col (col+1) row (row+1)
        setRowCol table labels newRow newCol isRow
            where
                newRow = row + (fromEnum (not isRow))
                newCol = col + (fromEnum isRow)


setInfoRow2Table :: Table -> InfoRows -> Int -> Int -> Bool -> IO ()
setInfoRow2Table _ [] _ _ _ = return ()
setInfoRow2Table table (iRow:infoRows) row col setRow =
    do
        setRowCol table iRow newRow newCol setRow
        setInfoRow2Table table infoRows (row + (fromEnum setRow)) (col + (fromEnum (not setRow))) setRow
            where
                newRow = if setRow then row else (row -(length iRow))
                newCol = if setRow then (col -(length iRow)) else col

setInfos2Table :: Table -> InfoRows -> InfoCols -> Int -> Int -> IO ()
setInfos2Table table infoRows infoCols rowMax colMax =
    do
        setInfoRow2Table table infoRows rowMax colMax True
        setInfoRow2Table table infoCols rowMax colMax False

createCorrectnees :: Int -> Int -> IO Correctness
createCorrectnees nRows nCols =
    do
        let matrix = [ [ False | i <- [0..(nRows-1)] ] | j <- [0..(nCols - 1)] ]
        sGame <- newEmptyMVar
        putMVar sGame False
        mvarMatrix <- newMVar matrix
        return ((Correctness mvarMatrix sGame))

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
        image <- (if rows > 9 then imageNewFromFile "LevelsFiles/FimDeJogoGrande.png" else imageNewFromFile "LevelsFiles/FimDeJogo.png")
        let field = createButtonField rows cols correctness solution table image
        let infoRows = createInfoListRow solution
        let infoCols = createInfoListCol solution
        setField2TableRow table field (halfInt rows) (halfInt cols)
        setInfos2Table table infoRows infoCols (halfInt rows) (halfInt cols)
        return (FullTable table field infoRows infoCols solution correctness image)


createInfoListRow :: [[Int]] -> [[IO Label]]
createInfoListRow matrix = map labelList $ reverse $ [reverse $ createLabel z [0] (-1) | z <- (reverse matrix)]

createInfoListCol :: [[Int]] -> [[IO Label]]
createInfoListCol matrix = map labelList [createLabel (map (!! n) (reverse matrix)) [0] (-1) | n <- [0..((length matrix)-1 )]]

createLabel :: [Int] -> [Int] -> Int -> [Int]
createLabel [] [0] _ = [0]
createLabel [] c _ = (init c)
createLabel (x:xs) (c:cs) y
  | x == 0    = createLabel xs (c:cs) (-1)
  | x == y    = createLabel xs ((c+1):cs) (x)
  | otherwise = createLabel xs (1:c:cs) (x)


labelList :: [Int] -> [IO Label]
labelList list = map (\s -> labelNew (Just (show s))) list

buttonFunction :: RowColButton -> Correctness -> Solution -> Table -> Image -> IO ()
buttonFunction (RowColButton button i j) correctness solution table image =
    do
        let (Correctness matrix endgame) = correctness
        bool <- readMVar endgame
        if bool then return ()
        else (do
            txt <- buttonGetLabel button
            newTxt <- transformTxt txt button
            buttonSetLabel button newTxt
            checkEndGame correctness (RowColButton button i j) solution
            let (Correctness matrix endgame) = correctness
            newBool <- readMVar endgame
            if newBool then stopGame table image (length solution) else return () )

transformTxt :: String -> Button -> IO String
transformTxt txt button =
    do
        if txt == " "
        then (do widgetModifyBg button StateNormal (Color 0 0 0); return "  ")
        else if txt == "  "
            then (do widgetModifyBg button StateNormal (Color 65535 65535 65535); return "X")
            else (do widgetModifyBg button StateNormal (Color 50000 50000 50000); return " ")

stopGame :: Table -> Image -> Int -> IO ()
stopGame table image len =
    do
        if len > 9
        then addComp2Table table image 0 7 0 8
        else addComp2Table table image 0 3 0 4
        widgetShow image
