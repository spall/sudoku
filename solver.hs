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

-- is sq1 a peer of sq2?
isPeer :: Square -> Square -> Bool
isPeer sq1 sq2 = True

-- returns as a list, the peers of this index
mkPeers :: Int -> Int -> Int -> [Int]
mkPeers m n i = let c = column m n i
                    r = row m n i in
                (rowPeers m n r) ++ (colPeers m n c) ++ (gridPeers m n (grid m n r c))

--given an index in a sudoku board of size m x n
-- gives row number that index belongs to.
row :: Int -> Int -> Int -> Int
row m n i = quot i (m*n) -- division

-- given an index in a sudoku board of size m x n
-- gives column number that index belongs to.
column :: Int -> Int -> Int -> Int
column m n i = let q = quot i (m*n) in
               i - q*m*n

-- given an index in sudoku board of size m x n
-- gives number of grid the index belongs to.
grid :: Int -> Int -> Int -> Int -> Int
grid m n r c = 0 -- incomplete

-- given a row number, returns as a list, the indices of that row
rowPeers :: Int -> Int -> Int -> [Int]
rowPeers m n r = [r*m*n..(r+1)*m*n-1]

-- given a column number, returns as a lsit, the indices of that column
colPeers :: Int -> Int -> Int -> [Int]
colPeers m n c = [c*i | i <- [0..m*n] ] -- map (* c) [0..m*n]

-- grid is n x m.
-- given a grid number, returns as a list, the indices of that grid
gridPeers :: Int -> Int -> Int -> [Int]
gridPeers m n g = [] -- incomplete

type SudokuBoard = Array Int Square

-- returns false if square can still be assigned a value, true otherwise
isZeroed :: Square -> Bool
isZeroed sq = let c_len = length (constraints sq) in
              (c_len == 0) && (not (squareSolved sq))

-- returns true if this is not a solution 
hasFailed :: SudokuBoard -> Bool
hasFailed board = foldl (\prev sq -> prev || (isZeroed sq)) False board

-- compares 2 squares constraint list lengths. returning the one with fewer constrains
sqCompare :: Square -> Square -> Square
sqCompare sq1 sq2 = let len = length (constraints sq1)
                        len2 = length (constraints sq2) in
                    if len == 0
                    then sq2
                    else if (len2 > 0) && len2 < len
                         then sq2
                         else sq1

-- returns index of square with fewest constraints
leastChoicesSquare :: SudokuBoard -> Square
leastChoicesSquare board = foldl sqCompare (board ! 0) board

solved :: SudokuBoard -> Bool
solved board = foldl (\prev sq -> prev && ((value sq) /= 0)) True board

-- removes value from list and returns new list
remove :: (Eq t) => t -> [t] -> [t]
remove i [] = []  
remove i (f:r) = if f == i
                 then r
                 else f:(remove i r)

-- update squares value and constraints of its peers
updateBoard :: SudokuBoard -> Int -> Int -> SudokuBoard
updateBoard board i val = let sq = board ! i
                              new_sq = sq { value = val, constraints = [] }
                              new_board = board//[(i, new_sq)] in
                          foldl (\board index -> let square = board ! index
                                                     new_c = remove val (constraints square)
                                                     new_sq2 = square { constraints = new_c } in
                                                  board//[(index, new_sq2)]) board (peers sq)


                          {-
                          amap (\square -> if isPeer square new_sq
                                           then let new_c = remove val (constraints square) in
                                                square { constraints = new_c }
                                           else square) new_board -}


ormap :: (Foldable a) => (t -> Bool) -> a t -> Bool
ormap func list = foldl (\prev val -> prev || (func val)) False list



search :: SudokuBoard -> Bool
search board = if hasFailed board
               then False
               else if solved board -- print board
                    then True
               else let lsq = leastChoicesSquare board in
                    ormap (\val -> (search (updateBoard board (pos lsq) val))) (constraints lsq)

{-                    
init :: SudokuBoard -> SudokuBoard -- do initial constraint propogation
init board = []
-}

main = return ()
