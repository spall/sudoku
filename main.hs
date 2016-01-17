module Main where

import System.IO
import System.IO.Error
import Solver
import Data.Array.IArray
import Data.Char
import GHC.IO.Handle
import GHC.IO.Handle.FD
import Control.Monad

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
            
            
-- (>>=) ::  m a -> (a -> m b) -> m b 
                  
-- ma >> mb -> mb
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

parseBoard  :: [String] -> [String]
parseBoard board = init (tail (tail board))

createBoard :: (Int,Int) -> [String] -> SudokuBoard
createBoard (m,n) str = if not (checkBoard (m,n) str)
                        then error "Error: board is of wrong size according to m and n"
                        else listArray (0,(m*n)^2-1)
                                       (map (\(s,i) -> let v = if all isDigit s
                                                               then read s::Int
                                                               else if (length s) == 1 &&  (head s) == '_'
                                                                    then 0
                                                                    else error "Invalid board format" in
                                                       mkSquare m n i v) (zip str [0..(m*n)^2-1]))

-- check if list length is correct
checkBoard :: (Int,Int) -> [String] -> Bool
checkBoard (m,n) strs = (length strs) == (m*n)^2

parseFile :: FilePath -> IO SudokuBoard
parseFile file = let handle = openFile file ReadMode -- handle is an IO handle
                     tokens = handle >>= getAllTokens in
                 liftM2 createBoard (liftM parseBoardSize tokens)
                                    (liftM parseBoard tokens)

printSolution :: Maybe SudokuBoard -> IO ()
printSolution Nothing = putStrLn (show "No solution")
printSolution (Just board) = putStrLn (printSudokuBoard board 3 3)

main = (liftM solve (parseFile "test.txt")) >>= printSolution

 -- problem is that iomn is not being evaluated so list_str has the board size
  -- (liftM solve (parseFile "test.txt")) >>= printSolution
       
           
  {-let test1 = "3 3 "++
                   "_ _ _ _ 4 8 3 _ _ "++
                   "_ _ _ 9 2 _ 5 _ _ "++
                   "2 4 1 _ _ _ 9 _ 7 "++
                   "1 _ _ 2 _ _ _ _ _ "++
                   "_ _ 7 8 _ 6 _ 4 _ "++
                   "3 _ 8 _ _ _ 6 5 9 "++
                   "8 7 _ 3 _ _ _ _ 5 "++
                   "_ _ 2 _ 9 _ 8 7 1 "++
                   "9 _ 5 _ _ _ 2 6 _ "
           str = words test
           m = read (head str)::Int
           n = read (head (tail str))::Int
           board = parseBoard m n (tail (tail str))

           test2 = "1 2 "++
                   "1 _ "++
                   "_ _ " in
       case (search (initConstraints board)) of
       Nothing -> putStrLn "No solution"
       Just board -> putStrLn (printSudokuBoard board m n) -}
     --  putStrLn (show (search (initConstraints board)))
