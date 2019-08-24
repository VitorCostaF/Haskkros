module Main where

import Graphics.UI.Gtk

import MainMenu
import Structures
import Defines

setWindowProps :: String -> Window -> IO ()
setWindowProps title window =
    set window [windowTitle := title, containerBorderWidth := 20,
                windowDefaultWidth := 500, windowDefaultHeight := 500]


main :: IO ()
main = createMainMenu
