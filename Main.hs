module Main where 

import Graphics.UI.Gtk
import Structures

setWindowProps :: String -> Window -> IO ()
setWindowProps title window = 
    set window [windowTitle := title, containerBorderWidth := 20,
                windowDefaultWidth := 500, windowDefaultHeight := 500]

 
main :: IO ()
main = do
    initGUI
    window <- windowNew
    setWindowProps "Step One" window

    --table <- createTable 5 5 
    TableField table field <- createTableWithField 5 5
    containerAdd window table

    button  <- getButtonFromTable (TableField table field) 5 5

    --let halfRow = 5 `div` 2 + 1 
    --let halfCol = 5 `div` 2 + 1
    --let field = createButtonField 5 5

    --setField2Table table field 5 5
    
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
