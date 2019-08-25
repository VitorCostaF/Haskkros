module Defines where

import Graphics.UI.Gtk
import Control.Concurrent
import Control.Monad

type ButtonField = [[IO RowColButton]]
type InfoRows = [[IO Label]]
type InfoCols = [[IO Label]]
type Solution = [[Int]]
type MatrixBool = [[Bool]]
type LevelID = String

data RowColButton = RowColButton Button Int Int 
data Correctness = Correctness (MVar MatrixBool) (MVar Bool)
data FullTable = FullTable Table ButtonField InfoRows InfoCols Solution Correctness Image