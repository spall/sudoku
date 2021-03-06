module Solver where

import Data.Array.IArray
import Data.List

data Square = Unsolved { constraints :: [Int]   -- possible values
                       , peers :: [Int]       -- squares that constrain this square and vice-versa
                       , pos :: Int
                       }
            | Solved {peers :: [Int]
                     , value :: Int
                     , pos :: Int
                     }

-- square constructor
          --  m -> n   -> index->val -> square
mkSquare :: Int -> Int -> Int -> Int -> Square
mkSquare m n i v = let peers = mkPeers m n i in
                   if v == 0
                   then Unsolved [1..m*n] peers i
                   else Solved peers v i

data Board t = Board { m_size :: Int
                     , n_size :: Int
                     , squares :: Array Int t
                     }

type SudokuBoard = Board Square

instance Eq Square where
  (==) Unsolved{} Solved{} = False
  (==) Solved{} Unsolved{} = False
  (==) Unsolved{} Unsolved{} = True
  (==) Solved{value=v1} Solved{value=v2} = v1 == v2

instance (Eq t) => Eq (Board t) where
  (==) Board{squares=sqs1} Board{squares=sqs2} = all (\(sq1,sq2) -> sq1 == sq2)
                                                (zip (elems sqs1) (elems sqs2))

instance Show Square where
  show Solved{value=v} = (show v)
  show Unsolved{} = "_"

instance (Show t) => Show (Board t) where
  show Board{m_size=m
            , n_size=n
            , squares=sqs} = foldl (\str i -> let is = rowPeers m n i
                                                  rs = (foldl
                                                        (\prev j -> let s = prev ++ (show (sqs ! j)) in
                                                                    if j /= 0 && mod (j+1) m == 0
                                                                    then s++"  "
                                                                    else s++ " ") str is) in
                                              if i /= 0 && mod (i+1) n == 0
                                              then rs++"\n\n"
                                              else rs++"\n") "" [0..(m*n-1)]


-- returns as a list, the peers of this index
mkPeers :: Int -> Int -> Int -> [Int]
mkPeers m n i = let c = column m n i
                    r = row m n i in
                delete i (nub $ (rowPeers m n r) ++ (colPeers m n c) ++ (gridPeers m n r c))

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
                    concatMap (\i -> [i*m*n+gc*m..(i*m*n+gc*m)+m-1]) [gr..gr+n-1]

isInvalid :: SudokuBoard -> Bool
isInvalid board@Board{squares=sqs} = any (\sq -> case sq of
                                                 Solved{peers=p
                                                       ,value=v} -> any
                                                                    (\p1 -> case sqs ! p1 of
                                                                            Solved{value=v2} -> v==v2
                                                                            Unsolved{} -> False) p
                                                 Unsolved{} -> False) sqs

-- returns true if board cannot be solved.
failed :: SudokuBoard -> Bool
failed Board{squares=sqs} = any (\sq -> case sq of
                                        Solved{} -> False
                                        Unsolved{constraints=c} -> null c) sqs

-- compares 2 squares constraint list lengths. returning the one with fewer constraints
sqCompare :: Square -> Square -> Square
sqCompare sq1@Solved{} sq2 = sq2
sqCompare sq1@Unsolved{} sq2@Solved{} = sq1
sqCompare sq1@Unsolved{constraints = c}
          sq2@Unsolved{constraints = c2} = if (length c2) < (length c)
                                           then sq2
                                           else sq1

-- returns square with fewest constraints
leastChoicesSquare :: SudokuBoard -> Square
leastChoicesSquare Board{squares=sqs} = foldl sqCompare (sqs ! 0) sqs

solved :: SudokuBoard -> Bool
solved Board{squares=sqs} = all (\sq -> case sq of
                           Solved{} -> True
                           Unsolved{} -> False) sqs

updateConstraints :: SudokuBoard -> [Int] -> Int -> SudokuBoard
updateConstraints board@Board{squares=sqs} list val = board{squares = (foldl (\b index -> let square = b ! index in
                                                                                          case square of
                                                                                          Solved{} -> b
                                                                                          Unsolved{constraints=c} -> b//[(index, square {constraints = (delete val c)})]) sqs list)}


-- update squares value and constraints of its peers
updateBoard :: SudokuBoard -> Int -> Int -> SudokuBoard
updateBoard board@Board{squares=sqs} i val = let sq = sqs ! i
                                                 new_sq =  Solved (peers sq) val i
                                                 new_board = board{squares = sqs//[(i, new_sq)]} in
                                             updateConstraints new_board (peers sq) val
                                             
-- can replace with builtin function?
maybeOrMap :: (Foldable a) => (t -> (Maybe b)) -> a t -> Maybe b
maybeOrMap func list = foldl (\prev val -> case prev of
                                           Nothing -> (func val)
                                           (Just board) -> (Just board)) Nothing list

search :: SudokuBoard -> Maybe SudokuBoard
search board = if failed board
                    then Nothing
                    else if solved board
                         then Just board
                         else let lsq = leastChoicesSquare board in
                              maybeOrMap (\val -> search (updateBoard board (pos lsq) val))
                                         (constraints lsq)


-- for each square in board, if its value is non-zero, do constraint propogation to peers
initConstraints :: SudokuBoard -> SudokuBoard
initConstraints board@Board{squares=sqs} = foldl (\b sq -> case sq of
                                                           Solved{value=v, peers=p} -> updateConstraints b p v
                                                           Unsolved{} -> b) board sqs

solve :: SudokuBoard -> Maybe SudokuBoard
solve board = if isInvalid board
              then Nothing
              else search (initConstraints board)
