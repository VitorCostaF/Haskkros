module Structures where

import Graphics.UI.Gtk

type ButtonField = [[IO Button]]

data TableField = TableField Table ButtonField

addButton2Table :: Table ->IO Button -> Int -> Int -> Int -> Int -> IO ()
addButton2Table table button lAtt rAtt tAtt bAtt = 
    do 
        aux <- button 
        tableAttachDefaults table aux lAtt rAtt tAtt bAtt

addUnitButton2Table :: Table -> IO Button -> Int -> Int -> IO ()
addUnitButton2Table table button row col = 
    addButton2Table table button row (row + 1) col (col + 1) 

getButtonFromField :: ButtonField -> Int -> Int -> IO Button
getButtonFromField field row col = (field !! (row-1)) !! (col-1)

createNewButton :: String -> IO Button
createNewButton str = 
    do 
        button <- buttonNewWithLabel " "
        widgetModifyBg button StateNormal (Color 65535 65535 65535)
        onClicked button (buttonFunction button)
        return button

createButtonField :: Int -> Int -> ButtonField
createButtonField rows cols =
    [[createNewButton "P" | i <-[1..rows]] | j <- [1..cols]]

halfInt :: Int -> Int
halfInt n = n `div` 2 + 1 

createTable :: Int -> Int -> IO Table
createTable rows cols =
    tableNew (rows + (halfInt rows)) (cols + (halfInt cols)) True

setField2TableCol :: Table -> [IO Button] -> Int -> Int -> IO ()
setField2TableCol table [button] i j = addUnitButton2Table table button i j
setField2TableCol table (button:buttons) i j= 
    do
        addUnitButton2Table table button i j
        setField2TableCol table buttons i (j+1)

setField2TableRow :: Table -> ButtonField -> Int -> Int -> IO ()
setField2TableRow table [part] i j = setField2TableCol table part i j
setField2TableRow table (part:field) i j = do
    setField2TableCol table part i j 
    setField2TableRow table field (i+1) j


setField2Table :: Table -> ButtonField -> Int -> Int -> IO ()
setField2Table table field rows cols= setField2TableRow table field (halfRow+1) (halfCol+1)
    where 
        halfRow = halfInt rows
        halfCol = halfInt cols

createTableWithField :: Int -> Int -> IO TableField 
createTableWithField rows cols = 
    do 
        let field = createButtonField rows cols
        table <- createTable rows cols
        setField2Table table field rows cols
        --enableFieldFunction field
        return (TableField table field)

getButtonFromTable :: TableField -> Int -> Int -> IO Button
getButtonFromTable (TableField table field) row col = 
    getButtonFromField field row col

buttonFunction :: Button -> IO ()
buttonFunction button = 
    do
        txt <- buttonGetLabel button
        let newTxt = case txt of
                        " " -> "  "
                        "  " -> "X"
                        "X" -> " "
        buttonSetLabel button newTxt
        
