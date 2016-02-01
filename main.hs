module Main where

import Client
import Test (run_all_tests, printSolution)
import System.Environment
import Server
            
main = getArgs >>= (\args -> if null args
                             then error "Error: no args"
                             else case head args of
                                  "--solve" -> if (length args) < 2
                                               then error "Error: missing file arg"
                                               else (getPuzzle (head (tail args))) >>= printSolution
                                  "--server" -> startServer
                                  "--test" -> run_all_tests)
