test : 
	cabal configure
	cabal build
	cp dist/build/guesser/guesser .

run : test
	./guesser

clean :
	rm -f -R dist
	rm guesser