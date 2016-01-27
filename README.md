A sudoku solver and generator written in Haskell as a course project.
Project requirements: http://www.cs.utah.edu/~mflatt/cs5965/sudoku.html

Dependencies: random package, happstack.server

Usage:

To run server: ./main --server

Then visit http://localhost:8000/?m=v&n=x

m and n specify board size M x N


To run client: ./main --solve url-to-board.txt

