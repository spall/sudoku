module Main where

import System.IO
import System.IO.Error
import Solver
import Parse
import Generator
import Test (run_all_tests, printSolution)
import Control.Monad

-- Command Line parsing ----
data Args = Args{ generate :: Maybe (Int,Int)
                , toSolve :: Maybe FilePath
                , run_tests :: Bool }

{-
run :: Args -> IO ()
run Args{generate=Nothing, solve=Nothing, run_tests=b2} = if not b2
                                                          then error "Must choose an option\n"
                                                          else return run_tests
run Args{generate=Nothing, }
-}
  
-- end command line parsing -------
main = (liftM solve (parseFile "test1.txt")) >>= printSolution

  --printSolution (liftM solve (parseFile "test1.txt"))

 -- (liftM show (generatePuzzle 3 3)) >>= putStrLn
  -- run_all_tests
  --(liftM solve (parseFile "test1.txt")) >>= printSolution
