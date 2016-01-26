module Client where

import Solver
import Parse
import Network.HTTP.Conduit
import Data.ByteString.Lazy.Char8

getPuzzle :: String -> IO SudokuBoard
getPuzzle path = (simpleHttp path) >>= (\bstr -> return (parseString (unpack bstr)))

