module Main where

import Client
import Test (run_all_tests, printSolution)
import System.Environment
            
main = getArgs >>= (\args -> if null args
                  then error "Error: no args"
                  else case head args of
                       "--solve" -> if (length args) < 2
                                    then error "Error: missing file arg"
                                    else (getPuzzle (head (tail args))) >>= printSolution)


 -- (liftM show (generatePuzzle 3 3)) >>= putStrLn
  -- run_all_tests
