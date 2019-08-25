module MainMenu where

import Graphics.UI.Gtk
import Control.Monad
import Structures
import Defines

--Numero de niveis totais
nbLvls :: Int
nbLvls = 10

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
    mainMenuTable <- tableNew (nbLvls+4) 12 True
    widgetModifyBg window StateNormal (Color 40000 10000 50000)
    createAboutButton nbLvls mainMenuTable window
    createTutorialButton mainMenuTable window
    tableAttachDefaults mainMenuTable labelTitle 0 12 0 1
    containerAdd window mainMenuTable
    mapM (createLevelButton mainMenuTable window) $ map show [1..(nbLvls)]


    onDestroy window mainQuit
    widgetShowAll window
    mainGUI

createAboutButton :: Int -> Table -> Window -> IO ()
createAboutButton nbLvls table window =
    do
        aboutTable <- tableNew 10 1 True
        button <- buttonNewWithLabel ("About")
        onClicked button (createAboutTable aboutTable window button)
        tableAttachDefaults table button 1 11 (nbLvls+4) (nbLvls+5)

createAboutTable :: Table -> Window -> Button -> IO ()
createAboutTable aboutTable window button =
    do
        labelLine1 <- labelNew (Just "Haskross")
        labelSetMarkup labelLine1 "<b><big><big><big><big>Haskross</big></big></big></big></b>"
        labelLine2 <- labelNew (Just "This game was created by Uematsu, F.; Lima, M. and Costa Farias, V.")
        labelLine3 <- labelNew (Just "as a project for the Programming Paradigms discipline in the")
        labelLine4 <- labelNew (Just "Computer Science course at the Federal University of ABC (Brazil).")
        tableAttachDefaults aboutTable labelLine1 0 1 0 1
        tableAttachDefaults aboutTable labelLine2 0 1 4 5
        tableAttachDefaults aboutTable labelLine3 0 1 5 6
        tableAttachDefaults aboutTable labelLine4 0 1 6 7

        widgetHideAll window
        createAboutWindow aboutTable
        widgetShowAll window


createAboutWindow :: Table -> IO ()
createAboutWindow aboutTable =
    do
        initGUI
        window <- windowNew
        widgetModifyBg window StateNormal (Color 60000 50000 60000)
        setWindowProps ("About") window
        containerAdd  window aboutTable
        onDestroy  window mainQuit
        widgetShowAll  window
        mainGUI

createTutorialButton :: Table -> Window -> IO ()
createTutorialButton mmtable window =
    do
        button <- buttonNewWithLabel ("Tutorial")
        tableAttachDefaults mmtable button 1 11 1 2

createLevelButton :: Table -> Window -> LevelID -> IO ()
createLevelButton mmtable window levelNb =
    do
        button <- buttonNewWithLabel ("Level " ++ levelNb)
        onClicked button (createLevelTable mmtable window levelNb button)
        let pos = ((read levelNb:: Int) + 2)
        let pos2 = ((read levelNb:: Int) + 3)
        tableAttachDefaults mmtable button 1 11 pos pos2

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
        widgetModifyBg window StateNormal (Color 60000 40000 60000)
        setWindowProps ("Level "++levelNb) window
        containerAdd window lvtable
        onDestroy window mainQuit
        widgetShowAll window
        mainGUI
