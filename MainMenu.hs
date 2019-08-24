module MainMenu where

import Graphics.UI.Gtk
import Control.Monad
import Structures
import Defines
type LevelID = String

setWindowProps :: String -> Window -> IO ()
setWindowProps title window =
    do
        set window [windowTitle := title, containerBorderWidth := 20,
                windowDefaultWidth := 700, windowDefaultHeight := 700]
        windowSetPosition window WinPosCenter


createMainMenu :: IO ()
createMainMenu = do
    initGUI
    window <- windowNew

    labelTitle <- labelNew (Just "Haskross")
    labelSetMarkup labelTitle "<b><big><big><big><big>Haskross</big></big></big></big></b>"


    setWindowProps "Main Menu" window
    mainMenuTable <- tableNew 14 12 True
    createAboutButton mainMenuTable window
    createTutorialButton mainMenuTable window
    tableAttachDefaults mainMenuTable labelTitle 1 11 0 1
    containerAdd window mainMenuTable
    mapM (createLevelButton mainMenuTable window) $ map show [1..9]


    onDestroy window mainQuit
    widgetShowAll window
    mainGUI

    --buttonTutorial <-
createAboutButton :: Table -> Window -> IO ()
createAboutButton mmtable window =
    do
        button <- buttonNewWithLabel ("About")
        --onClicked button (createAboutTable mmtable window levelNb button)
        tableAttachDefaults mmtable button 1 11 13 14
createTutorialButton :: Table -> Window -> IO ()
createTutorialButton mmtable window =
    do
        button <- buttonNewWithLabel ("Tutorial")
        --onClicked button (createTutorialTable mmtable window levelNb button)
        tableAttachDefaults mmtable button 1 11 1 2
createLevelButton :: Table -> Window -> LevelID -> IO ()
createLevelButton mmtable window levelNb =
    do
        button <- buttonNewWithLabel ("Level " ++ levelNb)
        onClicked button (createLevelTable mmtable window levelNb button)
        let positionY = ((read levelNb:: Int) + 2)
        let positionY2 = ((read levelNb:: Int) + 3)
        tableAttachDefaults mmtable button 1 11 positionY positionY2

createLevelTable :: Table -> Window -> LevelID -> Button -> IO()
createLevelTable mainMenuTable window levelNb button = do
    (FullTable lvtable field infoRows infoCols solution correctness image) <- createFullTable ("Level"++levelNb)
    widgetHideAll window
    createLevelWindow levelNb lvtable
    widgetShowAll window

createLevelWindow :: LevelID -> Table -> IO ()
createLevelWindow levelNb lvtable =
    do
        initGUI
        window <- windowNew
        setWindowProps ("Level "++levelNb) window
        containerAdd window lvtable
        onDestroy window mainQuit
        widgetShowAll window
        mainGUI
