module Server where

import Parse
import Happstack.Server 
import Control.Monad
import Control.Monad.Trans.Class
import Generator

getNums = liftM2 parse2Num (look "m") (look "n")

getResponse1 :: (Int,Int) -> IO String
getResponse1 a =  liftM show $ generatePuzzle a

realResponse :: (Int, Int) -> ServerPartT IO String
realResponse a = lift $ getResponse1 a

startServer :: IO ()
startServer = simpleHTTP nullConf $ getNums >>= realResponse >>= ok

 -- simpleHttp nullConf (getPuzzle "3" "3")

  --genBoard  >>= (\tmp -> tmp >>= (\gb -> simpleHTTP nullConf gb))
