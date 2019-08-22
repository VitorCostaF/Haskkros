module Main where 

import Graphics.UI.Gtk

import Structures 
import Defines

setWindowProps :: String -> Window -> IO ()
setWindowProps title window = 
    set window [windowTitle := title, containerBorderWidth := 20,
                windowDefaultWidth := 500, windowDefaultHeight := 500]

 
main :: IO ()
main = do
    initGUI
    window <- windowNew
    setWindowProps "Step One" window

    (FullTable table field infoRows infoCols solution correctness) <- createFullTable "Level1"
    containerAdd window table


    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
