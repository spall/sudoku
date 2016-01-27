module Server where

import Parse
import Happstack.Server 
import Control.Monad
import Generator

getPuzzle :: String -> String -> IO (ServerPart String)
getPuzzle m n = (liftM show (generatePuzzle (parse2Num m n))) >>= (\str -> return (ok str))

response:: IO String -> IO (ServerPart String)
response str = str >>= (\s -> return (ok s))

look2 :: String -> IO (ServerPart String)
look2 str = return (look str)

-- IO ServerPartT IO string
genBoard :: ServerPart (IO (ServerPart String))
genBoard = (look "m") >>= (\m -> (look "n") >>= (\n -> return (getPuzzle m n)))

  --(look2 "m") >>= (\m_ -> (look2 "n") >>= (\n_ -> m_ >>= (\m -> n_ >>= (\n -> getPuzzle m n))))
          
  --(return "") >>= (\e -> return ((look "m") >>= (\m -> return ((look "n") >>= (\n -> response (getPuzzle m n))))))

startServer :: ServerPart ()
startServer = simpleHttp nullConf (getPuzzle "3" "3")

  --genBoard  >>= (\tmp -> tmp >>= (\gb -> simpleHTTP nullConf gb))
