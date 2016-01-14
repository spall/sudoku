module Main where

import Solver
import Data.Array.IArray
import Data.Char


parseBoard :: Int -> Int -> [String] -> SudokuBoard
parseBoard m n str = listArray (0,(m*n)^2-1) (map (\(s,i) -> let v = if all isDigit s
                                                                     then read s::Int
                                                                     else if (length s) == 1 &&  (head s) == '_'
                                                                          then 0
                                                                          else error "Invalid board format" in
                                                             mkSquare m n i v) (zip str [0..(m*n)^2-1]))


main = let test = "3 3 "++
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
           board = parseBoard m n (tail (tail str)) in
       putStrLn (show (search (initConstraints board)))
