A sudoku solver and generator written in Haskell as a course project.

Project requirements:

Part 1. 

Implement a Sudoku solver. Your solver should at least work on 3×3 boards (i.e., 81 cells), but ideally it should work on an arbitrary M×N boards.

Also, implement a Sudoku board generator. The generator should generate a random board. (It should ideally be able to generate any possible Sudoku board that has a unique solution, but it’s not clear to the instructor that such a general generator is feasible.)

For the solver and generator part of the assignment, no particular input or output is required.

For the general M×N case, M and N do not have to be unique. An example solved 1x2 board is

  1  2
  2  1
and an example solved 3x2 board is

  1 2 3  4 5 6
  4 5 6  1 2 3
  
  2 3 4  5 6 1
  5 6 1  2 3 4
  
  3 4 5  6 1 2
  6 1 2  3 4 5

Part 2. 

Create client and server wrappers for your Sudoku solver.

The client should take a URL and download a board from that site, where the URL will always end in ".txt" and the protocol is HTTP 1.0. The server should provide a board via HTTP 1.0 as unencoded ASCII.

A served board is a sequence of numbers (composed of digits 0 through 9) and _, where any amounf of whitespace (space, newline, carriage return, or tab) separate the numbers and _ tokens. The first two items represent the board dimensions M and N, and the rest of the items represent the board content, from left to right and then top to bottom, where _ represents an empty cell. Non-blank cells have numbers from 1 (inclusive) to M×N (exclusive).

For example, a Sudoku-board server might return the following content:

  3 3
  _ _ _ _ 4 8 3 _ _
  _ _ _ 9 2 _ 5 _ _
  2 4 1 _ _ _ 9 _ 7
  1 _ _ 2 _ _ _ _ _
  _ _ 7 8 _ 6 _ 4 _
  3 _ 8 _ _ _ 6 5 9
  8 7 _ 3 _ _ _ _ 5
  _ _ 2 _ 9 _ 8 7 1
  9 _ 5 _ _ _ 2 6 _
