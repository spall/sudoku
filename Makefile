all: client.hs server.hs solver.hs generator.hs parse.hs test.hs main.hs	
	ghc server.hs client.hs solver.hs generator.hs parse.hs test.hs main.hs

clean:
	rm *.o *.hi main
