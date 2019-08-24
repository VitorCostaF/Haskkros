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
                windowDefaultWidth := 500, windowDefaultHeight := 500]
        windowSetPosition window WinPosCenter


createMainMenu :: IO ()
createMainMenu = do
    initGUI
    window <- windowNew

    setWindowProps "Main Menu" window
    mainMenuTable <- tableNew 12 12 True
    containerAdd window mainMenuTable
    mapM (createLevelButton mainMenuTable window) $ map show [1..9]
    --createLevelButton mainMenuTable window "1"


    onDestroy window mainQuit
    widgetShowAll window
    mainGUI

    --buttonTutorial <-

createLevelButton :: Table -> Window -> LevelID -> IO ()
createLevelButton mmtable window levelNb =
    do
        button <- buttonNewWithLabel ("Level " ++ levelNb)
        onClicked button (createLevelTable mmtable window levelNb button)
        let positionY = ((read levelNb:: Int) + 1)
        let positionY2 = ((read levelNb:: Int) + 2)
        tableAttachDefaults mmtable button 1 11 positionY positionY2

createLevelTable :: Table -> Window -> LevelID -> Button -> IO()
createLevelTable mainMenuTable window levelNb button = do
    --set window [windowTitle := ("Level "++levelNb)]
    (FullTable lvtable field infoRows infoCols solution correctness image) <- createFullTable ("Level"++levelNb)
    widgetHideAll window
    createLevelWindow levelNb lvtable
    widgetShowAll window

returnToMainMenu :: Table -> Table-> Window -> LevelID -> IO ()
returnToMainMenu mmtable mainMenuTable window levelNb = do
    set window [windowTitle := "Main Menu"]
    containerRemove window mmtable
    containerAdd    window mainMenuTable

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


{-createLevelButtonsFunctions :: Table -> Window -> LevelID ->  IO ()
createLevelButtonsFunctions mmtable window levelNb=
    map (createLevelButton window levelNb ) (levelButtonField createLevelButton mmtable window levelNb)-}

{-levelFunctions :: Button -> Table -> LevelId -> IO ()
levelFunctions btn mmtable = do
    let positionY = (show $ (read levelNb:: Int) + 1)
    let positionY2 = (show $ (read levelNb:: Int) + 2)
    tableAttachDefaults mmtable button 1 11 positionY positionY2
  -}
{-levelButtonField :: (Table -> Window -> LevelID -> IO ()) -> Table -> Window -> LevelID -> [Button]
levelButtonField createLevelButton mmtable window levelNb =
    [createLevelButton mmtable window levelNb | i <-[1..9]]
-}
