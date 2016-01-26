module Parse where

import Solver
import Data.Array.IArray
import Data.Char
import GHC.IO.Handle
import GHC.IO.Handle.FD
import Control.Monad
import System.IO
import System.IO.Error
import Text.Printf

eatSpace :: Handle -> IO ()
eatSpace h = liftM2 (||) (hIsEOF h)
                         (liftM (not.isSpace)
                          (catchIOError (hLookAhead h)
                           (\e -> if isEOFError e
                                  then return 'c'
                                  else (hLookAhead h)))) >>= (\b -> if b
                                                                    then return ()
                                                                    else (hGetChar h) >> (eatSpace h))

getWord :: Handle -> IO String
getWord h = liftM2 (||) (hIsEOF h)
                        (liftM isSpace
                         (catchIOError (hLookAhead h)
                          (\e -> if isEOFError e
                                 then return ' '
                                 else (hLookAhead h)))) >>= (\b -> if b
                                                                   then return ""
                                                                   else liftM2 (:) (hGetChar h) (getWord h))

nextToken :: Handle -> IO String
nextToken handle = (eatSpace handle) >> (getWord handle)

getAllTokens :: Handle -> IO [String]
getAllTokens handle = (hIsEOF handle) >>= (\b -> if b 
                                                 then return []
                                                 else liftM2 (:) (nextToken handle)
                                                                 (getAllTokens handle))

parse2Num :: String -> String -> (Int,Int)
parse2Num n1 n2 = if (not (all isDigit n1)) || (not (all isDigit n2))
                  then error "Both strings are not integers"
                  else (read n1::Int, read n2::Int)

parseBoardSize :: [String] -> (Int,Int)
parseBoardSize strs = parse2Num (head strs) (head (tail strs))

parseBoard  :: [String] -> [String] -- does init becuase board has an "" at the end we don't want.
parseBoard board = init (tail (tail board))

createBoard :: (Int,Int) -> [String] -> SudokuBoard
createBoard (m,n) str = if not (checkBoard (m,n) str)
                        then error (printf "Error: board is of wrong size according to m and n. Wanted board of length %d, got %d.\n" ((m*n)^2) (length str))
                        else Board m n (listArray (0,(m*n)^2-1)
                                        (map (\(s,i) -> let v = if all isDigit s
                                                                then read s::Int
                                                                else if (length s) == 1 &&  (head s) == '_'
                                                                     then 0
                                                                     else error "Invalid board format" in
                                                        mkSquare m n i v) (zip str [0..(m*n)^2-1])))

-- check if list length is correct
checkBoard :: (Int,Int) -> [String] -> Bool
checkBoard (m,n) strs = (length strs) == (m*n)^2

-- parse string to sudokuboard
parseString :: String -> SudokuBoard
parseString test = let strs = words test
                       size = parseBoardSize strs
                       board = tail (tail strs) in
                   (createBoard size board)

-- parse file to io sudokuboard
parseFile :: FilePath -> IO SudokuBoard
parseFile file = let handle = openFile file ReadMode -- handle is an IO handle
                     tokens = handle >>= getAllTokens in
                 liftM2 createBoard (liftM parseBoardSize tokens)
                                    (liftM parseBoard tokens)
