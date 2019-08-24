module Defines where

import Graphics.UI.Gtk

import Data.Char
import Control.Concurrent
import Control.Monad
--import System.IO



type ButtonField = [[IO RowColButton]]
type InfoRows = [[IO Label]]
type InfoCols = [[IO Label]]
type Solution = [[Int]]
type MatrixBool = [[Bool]]

data RowColButton = RowColButton Button Int Int 
data Correctness = Correctness (MVar MatrixBool) (MVar Bool)
data FullTable = FullTable Table ButtonField InfoRows InfoCols Solution Correctness Image