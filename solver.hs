module Solver where

import Data.Array.IArray

data Square = Unsolved { constraints :: [Int]   -- possible values
                       , peers :: [Int]       -- squares that constrain this square and vice-versa
                       , pos :: Int
                       }
            | Solved {peers :: [Int]
                     , value :: Int
                     , pos :: Int
                     } deriving (Show)

type SudokuBoard = Array Int Square


-- A sudoku grid (small square) has a list of contraints (numbers it could be)
-- and a list of "peers" squares that cannot have the same number.

ormap :: (Foldable a) => (t -> Bool) -> a t -> Bool
ormap func list = foldl (\prev val -> prev || (func val)) False list

-- removes value from list and returns new list
remove :: (Eq t) => t -> [t] -> [t]
remove i [] = []  
remove i (f:r) = if f == i
                 then r
                 else f:(remove i r)

-- square constructor
          --  m -> n   -> index->val -> square
mkSquare :: Int -> Int -> Int -> Int -> Square
mkSquare m n i v = let peers = mkPeers m n i in
                   if v == 0
                   then Unsolved [1..m*n] peers i
                   else Solved peers v i

-- returns as a list, the peers of this index
mkPeers :: Int -> Int -> Int -> [Int]
mkPeers m n i = let c = column m n i
                    r = row m n i in
                (rowPeers m n r) ++ (colPeers m n c) ++ (gridPeers m n r c)

--given an index in a sudoku board of size m x n
-- gives row number that index belongs to.
row :: Int -> Int -> Int -> Int
row m n i = quot i (m*n) -- division,  row can be 0

-- given an index in a sudoku board of size m x n
-- gives column number that index belongs to.
column :: Int -> Int -> Int -> Int
column m n i = i - (quot i (m*n))*m*n -- column can be 0

-- given a row number, returns as a list, the indices of that row
rowPeers :: Int -> Int -> Int -> [Int]
rowPeers m n r = [r*m*n..(r+1)*m*n-1] -- looks like it works

-- given a column number, returns as a list, the indices of that column
colPeers :: Int -> Int -> Int -> [Int]
colPeers m n c = [c+ n*m*i | i <- [0..m*n-1]] -- map (* c) [0..m*n] // does not work if c is 0.

-- should test this function....
-- grid is n x m.
-- given a grid number, returns as a list, the indices of that grid
gridPeers :: Int -> Int -> Int -> Int -> [Int]
gridPeers m n r c = let gc = quot c m
                        gr = n * (quot r n) in
                  foldl (\prev i -> prev ++ [i*m*n+gc*m..(i*m*n+gc*m)+m-1]) [] [gr..gr+n-1]



-- returns returns true if constraint list is empty and is unsolved
sqFailed :: Square -> Bool
sqFailed Solved{} = False
sqFailed Unsolved {constraints = c } = null c

-- returns true if this is not a solution 
hasFailed :: SudokuBoard -> Bool
hasFailed board = ormap sqFailed board

-- compares 2 squares constraint list lengths. returning the one with fewer constraints
sqCompare :: Square -> Square -> Square
sqCompare sq1@Solved{} sq2 = sq2
sqCompare sq1@Unsolved{} sq2@Solved{} = sq1
sqCompare sq1@Unsolved{constraints = c} sq2@Unsolved{constraints = c2} = let len = length c
                                                                             len2 = length c2 in
                                                                         if len2 < len
                                                                         then sq2
                                                                         else sq1

-- returns index of square with fewest constraints
leastChoicesSquare :: SudokuBoard -> Square
leastChoicesSquare board = foldl sqCompare (board ! 0) board


solved :: SudokuBoard -> Bool
solved board = foldl (\prev sq -> prev && case sq of
                                          Solved{} -> True
                                          Unsolved{} -> False) True board

updateConstraints :: SudokuBoard -> [Int] -> Int -> SudokuBoard
updateConstraints board list val = foldl (\b index -> let square = b ! index in
                                                      case square of
                                                      Solved{} -> b
                                                      Unsolved{constraints=c} -> b//[(index, square {constraints = (remove val c)})]) board list


-- update squares value and constraints of its peers
updateBoard :: SudokuBoard -> Int -> Int -> SudokuBoard
updateBoard board i val = let sq = board ! i
                              new_sq =  Solved (peers sq) val i
                              new_board = board//[(i, new_sq)] in
                          updateConstraints new_board (peers sq) val

                          {-
                          amap (\square -> if isPeer square new_sq
                                           then let new_c = remove val (constraints square) in
                                                square { constraints = new_c }
                                           else square) new_board -}

toBool :: Maybe SudokuBoard -> Bool
toBool Nothing = False
toBool (Just board) = True

maybeOr :: Maybe b -> Maybe b-> Maybe b
maybeOr b1@Nothing b2 = b2
maybeOr b1@(Just thing) b2 = b1

maybeOrMap :: (Foldable a) => (t -> (Maybe b)) -> a t -> Maybe b
maybeOrMap func list = foldl (\prev val -> maybeOr prev (func val)) Nothing list

search :: SudokuBoard -> Maybe SudokuBoard
search board = if hasFailed board
               then Nothing
               else if solved board -- print board
                    then Just board
               else let lsq = leastChoicesSquare board in
                    maybeOrMap (\val -> (search (updateBoard board (pos lsq) val))) (constraints lsq)


-- for each square in board, if its value is non-zero, do constraint propogation to peers
initConstraints :: SudokuBoard -> SudokuBoard
initConstraints board = foldl (\b sq -> case sq of
                                        Solved{value=v, peers=p} -> updateConstraints b p v
                                        Unsolved{} -> b) board board

{-                    
init :: SudokuBoard -> SudokuBoard -- do initial constraint propogation
init board = []
-}

-- TODO
-- 2. parse string boards into SudokuBoard data structure.
-- 3. Be able to print out solution sudoku board
-- 4. Test cases

printSudokuBoard :: SudokuBoard -> Int -> Int -> String
printSudokuBoard board m n = foldl (\str i -> let is = rowPeers m n i
                                                  rs = (foldl (\prev j -> let s = prev++(show (value (board ! j))) in
                                                                          if j /= 0 && mod (j+1) m == 0
                                                                          then s++"  "
                                                                          else s++ " ") str is) in
                                              if i /= 0 && mod (i+1) n == 0
                                              then rs++"\n\n"
                                              else rs++"\n") "" [0..(m*n-1)]
