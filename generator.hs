module Generator where

import Solver
import Data.Array.IArray
import System.Random
import Control.Monad
import Text.Printf

randomNumber :: Int -> Int -> IO Int
randomNumber start end = getStdRandom (randomR (start,end))

inListPair :: (Eq t1) => t1 -> [(t1,t2)] -> Bool
inListPair index list = any (\(i,v) -> i == index) list

mkPair :: Int -> Int -> (Int,Int)
mkPair f s = (f,s)

concatLists :: [t] -> [t] -> [t]
concatLists l1 l2 = l1 ++ l2
                                
listRandomNumbers :: Int -> Int -> IO [(Int,Int)]
listRandomNumbers m n = let func = (\rest i -> let index = randomNumber 0 ((m*n)^2-1) in
                                               liftM2 inListPair index rest >>= (\b -> if b
                                                                                       then func rest i
                                                                                       else liftM2 (:) (liftM2 mkPair index (randomNumber 1 (m*n))) rest)) in
                        foldl func (return []) [1..m*n]

seedBoard :: Int -> Int -> IO SudokuBoard
seedBoard m n = (listRandomNumbers m n) >>= (\pairs -> return (Board m n (array (0,(m*n)^2-1)
                                                                         (map (\(i,v) -> (i, mkSquare m n i v))
                                                                          (pairs ++ (filter (\(i,v) -> not (inListPair i pairs))
                                                                                     [(i,0) | i <- [0..(m*n)^2-1]]))))))
hasUniqueSolution :: SudokuBoard -> Bool
hasUniqueSolution board = 1 == (foldl (\sum sol -> case sol of
                                                    Nothing -> sum
                                                    (Just board) -> sum+1) 0 (searchAllSolutions board))

searchAllSolutions :: SudokuBoard -> [Maybe SudokuBoard]
searchAllSolutions board = if failed board
                          then [Nothing]
                          else if solved board
                               then [Just board]
                               else let lsq = leastChoicesSquare board in
                                    foldl (\prev val -> prev++(searchAllSolutions (updateBoard board (pos lsq) val)))
                                    [] (constraints lsq)

canRemove :: Square -> Bool
canRemove Unsolved{} = False
canRemove Solved{} = True

inList :: (Eq t) => t -> [t] -> Bool
inList val [] = False
inList val (f:r) = (val == f) || (inList val r)

addToList :: (Eq t) => t -> [t] -> [t]
addToList v list = if inList v list
                   then list
                   else v:list

addConstraints :: SudokuBoard -> [Int] -> Int -> SudokuBoard
addConstraints board@Board{squares=sqs} list val = board{squares = (foldl (\b index -> let square = b ! index  in
                                                                                       case square of
                                                                                       Solved{} -> b
                                                                                       Unsolved{constraints=c} -> b//[(index, square{constraints = (addToList val c)})]) sqs list)}

-- keeps removing squares until it finds one that leaves the board with only 1 solution.
-- returns nothing if not possible to remove a square
removeSquare :: SudokuBoard -> [Int] -> Int -> IO (Maybe SudokuBoard)
removeSquare board tried 0 = return Nothing
removeSquare board@Board{m_size=m
                        , n_size=n
                        , squares=sqs}
  tried
  count = (randomNumber 0 ((m*n)^2-1)) >>= (\index -> case sqs ! index of
                                                      Unsolved{} -> removeSquare board tried count
                                                      Solved{peers=p, value=v, pos=i} -> if inList index tried
                                                                                         then removeSquare board tried count
                                                                                         else let tmp_board = board{squares=(sqs//[(index,Unsolved[1..m*n] p index)])}
                                                                                                  new_board = addConstraints (initConstraints tmp_board) p v in
                                                                                              if not (hasUniqueSolution new_board)
                                                                                              then removeSquare board (index:tried) (count-1)
                                                                                              else return (Just new_board))

countSolved :: SudokuBoard -> Int
countSolved Board{squares=sqs} = foldl (\sum s -> case s of
                                                  Solved{} -> sum+1
                                                  Unsolved{} -> sum) 0 sqs

removeAllSquares :: SudokuBoard -> IO SudokuBoard
removeAllSquares board = (removeSquare board [] (countSolved board)) >>= (\b -> case b of
                                                                                Nothing -> return board
                                                                                (Just b2) -> removeAllSquares b2)

generatePuzzle :: Int -> Int -> IO SudokuBoard
generatePuzzle m n = liftM solve (seedBoard m n) >>= (\b -> case b of
                                                            Nothing -> generatePuzzle m n
                                                            (Just board) -> removeAllSquares board)
                     

                       
