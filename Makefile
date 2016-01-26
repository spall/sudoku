all: solver.hs generator.hs parse.hs test.hs main.hs	
	ghc client.hs solver.hs generator.hs parse.hs test.hs main.hs

clean:
	rm *.o *.hi main
