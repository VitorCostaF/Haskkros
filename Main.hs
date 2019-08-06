module Main where 

import Graphics.UI.Gtk
import Structures

setWindowProps :: String -> Window -> IO ()
setWindowProps title window = 
    set window [windowTitle := title, containerBorderWidth := 20,
                windowDefaultWidth := 500, windowDefaultHeight := 500]

addButton2Table2 :: Table -> Button -> Int -> Int -> Int -> Int -> IO ()
addButton2Table2 table button lAtt rAtt tAtt bAtt =
        tableAttachDefaults table button lAtt rAtt tAtt bAtt

addUnitButton2Table2 :: Table -> Button -> Int -> Int -> IO ()
addUnitButton2Table2 table button row col = 
    addButton2Table2 table button row (row + 1) col (col + 1) 

setField2Table2 :: Table -> Int -> Int -> [[IO ()]]
setField2Table2 table rows cols =
    [[addUnitButton2Table table (buttonNewWithLabel " ") (halfRow + i + 1) (halfCol + j + 1)| i<-[0..(rows-1)] ] | j <-[0..(cols-1)]]
        where 
            halfRow = rows `div` 2 + 1 
            halfCol = cols `div` 2 + 1
 
main :: IO ()
main = do
    initGUI
    window <- windowNew
    setWindowProps "Step One" window

    --table <- createTable 5 5 
    table <- createTableWithField 5 5
    containerAdd window table

    --let halfRow = 5 `div` 2 + 1 
    --let halfCol = 5 `div` 2 + 1
    --let field = createButtonField 5 5

    --setField2Table table field 5 5
    
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
