module Client where

import Solver
import Parse
import Network.HTTP.Conduit
import Data.ByteString.Lazy.Char8
import Control.Monad

getPuzzle :: String -> IO (Maybe SudokuBoard)
getPuzzle path = liftM solve ((simpleHttp path) >>= (\bstr -> return (parseString (unpack bstr))))

