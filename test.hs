module Test where

import Solver
import Parse

run_test1 :: IO ()
run_test1 = let test = " 3 3 "++         -- has a solution
                       "_ _ _ _ 4 8 3 _ _ "++
                       "_ _ _ 9 2 _ 5 _ _ "++
                       "2 4 1 _ _ _ 9 _ 7 "++
                       "1 _ _ 2 _ _ _ _ _ "++
                       "_ _ 7 8 _ 6 _ 4 _ "++
                       "3 _ 8 _ _ _ 6 5 9 "++
                       "8 7 _ 3 _ _ _ _ 5 "++
                       "_ _ 2 _ 9 _ 8 7 1 "++
                       "9 _ 5 _ _ _ 2 6 _"
                sol = " 3 3 "++
                      "7 5 9  1 4 8  3 2 6 "++
                      "6 8 3  9 2 7  5 1 4 "++
                      "2 4 1  5 6 3  9 8 7 "++
                      "1 6 4  2 5 9  7 3 8  "++
                      "5 9 7  8 3 6  1 4 2  "++
                      "3 2 8  4 7 1  6 5 9  "++
                      "8 7 6  3 1 2  4 9 5  "++
                      "4 3 2  6 9 5  8 7 1  "++
                      "9 1 5  7 8 4  2 6 3 " in
            (putStrLn "\nTest 1: ") >> (putStrLn (boolToString (compareBoards
                                                                (solve (parseTest test))
                                                                (Just (parseTest sol)))))

run_test2 :: IO ()
run_test2 = let test = " 4 3 "++                      -- has a solution
                       "_ _  2  3 _ _ _ _ _ _  _ 8 "++
                       "8 _  _  _ _ _ 5 _ _ 10 _ 7 "++
                       "_ 12 10 4 _ _ 9 8 1 _  _ _ "++
                       "3 6  12 5 _ _  2 9 _ _  _ _ "++
                       "_ 7  4  _ 1 11 10 _ _ _ 3 _ "++
                       "_ _ _ _ 12 _ _ _ _ 7 _ _ "++
                       "_ _ 6 _ _ _ _ 7 _ _ _ _ "++
                       "_ 8 _ _ _ 10 12 11 _ 2 6 _ "++
                       "_ _ _ _ 8 6 _ _ 7 5 9 10 "++
                       "_ _ _ 2 5 4 _ _ 9 12 10 _ "++
                       "11 _ 1 _ _ 12 _ _ _ _ _ 5 "++
                       "12 _ _ _ _ _ _ _ 8 4 _ _ "
                sol = " 4 3 "++
                      "5 1 2 3 11 7 4 10  6 9 12 8 "++
                      "8 9 11 6   3 1 5 12   4 10 2 7 "++  
                      "7 12 10 4  6 2 9 8    1 11 5 3 "++
                      "3 6 12 5   7 8 2 9     10 1 11 4 "++
                      "2 7 4 8   1 11 10 5    12 6 3 9  "++
                      "1 10 9 11  12 3 6 4   5 7 8 2  "++
                      "10 2 6 1  9 5 3 7  11 8 4 12  "++
                      "9 8 5 7  4 10 12 11  3 2 6 1  "++
                      "4 11 3 12  8 6 1 2  7 5 9 10  "++
                      "6 3 8 2  5 4 7 1  9 12 10 11  "++
                      "11 4 1 9  10 12 8 6  2 3 7 5  "++
                      "12 5 7 10  2 9 11 3  8 4 1 6" in
            (putStrLn "\nTest 2: ") >> (putStrLn (boolToString (compareBoards
                                                                (solve (parseTest test))
                                                                (Just (parseTest sol)))))

run_test3 :: IO ()
run_test3 = let test = "1 2 "++
                       "1 _ "++
                       "_ _"
                sol = "1 2 "++
                      "1 2 "++
                      "2 1 " in
            (putStrLn "\nTest 3: ") >> (putStrLn (boolToString (compareBoards
                                                                (solve (parseTest test))
                                                                (Just (parseTest sol)))))

run_test4 :: IO ()
run_test4 = let test = " 3 3 "++         -- has no solution
                       "_ _ _  _ 4 8  3 _ _ "++
                       "_ _ _  9 2 _  5 _ _ "++
                       "2 4 1  _ _ _  9 _ 7 "++
                       "1 _ _  2 _ _  _ _ _ "++
                       "_ _ 7  9 _ 6  _ 4 _ "++
                       "3 _ 8  _ _ _  6 5 9 "++
                       "8 7 _  3 _ _  _ _ 5 "++
                       "_ _ 2  _ 9 _  8 7 1 "++
                       "9 _ 5  _ _ _  2 6 _" in
            (putStrLn "\nTest 4: ") >> (putStrLn (boolToString (compareBoards
                                                                (solve (parseTest test))
                                                                Nothing)))

boolToString :: Bool -> String
boolToString False = "Failed"
boolToString True = "Passed"

compareBoards :: Maybe SudokuBoard -> Maybe SudokuBoard -> Bool
compareBoards Nothing Nothing = True
compareBoards (Just board1) (Just board2) = board1 == board2
compareBoards Nothing (Just board2) = False
compareBoards (Just board1) Nothing = False

parseTest :: String -> SudokuBoard
parseTest test = let strs = words test
                     size = parseBoardSize strs
                     board = tail (tail strs) in
                 (createBoard size board)

printSolution :: Maybe SudokuBoard -> IO ()
printSolution Nothing = putStrLn (show "No solution")
printSolution (Just board) = putStrLn (show board)
 
run_all_tests :: IO ()
run_all_tests = run_test1 >> run_test2 >> run_test3 >> run_test4




  
