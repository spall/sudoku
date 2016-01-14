module Main where

import Data.Array.IArray

{-
class Nullable t where
  isNull :: t -> Bool

instance Nullable Int where
  isNull i = i == 0

instance Nullable Char where
  isNull c = c == '\0'
-}

-- A sudoku grid (small square) has a list of contraints (numbers it could be)
-- and a list of "peers" squares that cannot have the same number.

-- Need a search function

-- Need a function to update constraints

-- Need a sudoku board representation
-- Use an Array. Where index using Integer, and then have a data time in the array.
-- data type can store the constraints and to save doing math could store index of peers

--type IntSquare = Square Int

ormap :: (Foldable a) => (t -> Bool) -> a t -> Bool
ormap func list = foldl (\prev val -> prev || (func val)) False list

-- removes value from list and returns new list
remove :: (Eq t) => t -> [t] -> [t]
remove i [] = []  
remove i (f:r) = if f == i
                 then r
                 else f:(remove i r)

data Square = Square { constraints :: [Int]   -- possible values
                       , peers :: [Int]       -- squares that constrain this square and vice-versa
                       , value :: Int         -- value of this square. 0 if undecided.
                       , pos :: Int
                       } deriving (Show)


-- square constructor
          --  m -> n   -> index->val -> square
mkSquare :: Int -> Int -> Int -> Int -> Square
mkSquare m n i v= let c = if v == 0 then [] else [1..m*n]
                      p = mkPeers i m n in
                   Square c p v i

squareSolved :: Square -> Bool
squareSolved sq = (value sq) /= 0

-- returns as a list, the peers of this index
mkPeers :: Int -> Int -> Int -> [Int]
mkPeers m n i = let c = column m n i
                    r = row m n i in
                (rowPeers m n r) ++ (colPeers m n c) ++ (gridPeers m n r c)

--given an index in a sudoku board of size m x n
-- gives row number that index belongs to.
row :: Int -> Int -> Int -> Int
row m n i = quot i (m*n) -- division

-- given an index in a sudoku board of size m x n
-- gives column number that index belongs to.
column :: Int -> Int -> Int -> Int
column m n i = i - (quot i (m*n))*m*n

-- given a row number, returns as a list, the indices of that row
rowPeers :: Int -> Int -> Int -> [Int]
rowPeers m n r = [r*m*n..(r+1)*m*n-1]

-- given a column number, returns as a lsit, the indices of that column
colPeers :: Int -> Int -> Int -> [Int]
colPeers m n c = [c*i | i <- [0..m*n] ] -- map (* c) [0..m*n]

-- grid is n x m.
-- given a grid number, returns as a list, the indices of that grid
gridPeers :: Int -> Int -> Int -> Int -> [Int]
gridPeers m n r c = let gc = quot c m
                      gr = n * (quot r n) in
                  foldl (\prev i -> prev ++ [i*m*n+gc*m..(i*m*n+gc*m)+m-1]) [] [gr..gr+n-1]

type SudokuBoard = Array Int Square

-- returns false if square can still be assigned a value, true otherwise
isZeroed :: Square -> Bool
isZeroed sq = null (constraints sq) && (not (squareSolved sq))

-- returns true if this is not a solution 
hasFailed :: SudokuBoard -> Bool
hasFailed board = ormap isZeroed board

-- compares 2 squares constraint list lengths. returning the one with fewer constraints
sqCompare :: Square -> Square -> Square
sqCompare sq1 sq2 = if null (constraints sq1)
                    then sq2
                    else let len = length (constraints sq1)
                             len2 = length (constraints sq2) in
                         if (len2 > 0) && len2 < len
                         then sq2
                         else sq1

-- returns index of square with fewest constraints
leastChoicesSquare :: SudokuBoard -> Square
leastChoicesSquare board = foldl sqCompare (board ! 0) board

solved :: SudokuBoard -> Bool
solved board = foldl (\prev sq -> prev && ((value sq) /= 0)) True board

updateConstraints :: SudokuBoard -> [Int] -> Int -> SudokuBoard
updateConstraints board list val = foldl (\b index -> let square = b ! index
                                                          new_c = remove val (constraints square)
                                                          new_sq2 = square { constraints = new_c } in
                                                      b//[(index, new_sq2)]) board list

-- update squares value and constraints of its peers
updateBoard :: SudokuBoard -> Int -> Int -> SudokuBoard
updateBoard board i val = let sq = board ! i
                              new_sq = sq { value = val, constraints = [] }
                              new_board = board//[(i, new_sq)] in
                          updateConstraints new_board (peers sq) val

                          {-
                          amap (\square -> if isPeer square new_sq
                                           then let new_c = remove val (constraints square) in
                                                square { constraints = new_c }
                                           else square) new_board -}




search :: SudokuBoard -> Bool
search board = if hasFailed board
               then False
               else if solved board -- print board
                    then True
               else let lsq = leastChoicesSquare board in
                    ormap (\val -> (search (updateBoard board (pos lsq) val))) (constraints lsq)


-- for each square in board, if its value is non-zero, do constraint propogation to peers
initConstraints :: SudokuBoard -> SudokuBoard
initConstraints board = foldl (\b sq -> if squareSolved sq
                                        then let val = value sq in
                                             updateConstraints b (peers sq) val
                                        else b) board board

{-                    
init :: SudokuBoard -> SudokuBoard -- do initial constraint propogation
init board = []
-}

-- TODO
-- 1. finish grid functions.  Or come up with new way to represent sudoku board in array
-- 2. parse string boards into SudokuBoard data structure.
-- 3. Be able to print out solution sudoku board
-- 4. Test cases

main = return ()
