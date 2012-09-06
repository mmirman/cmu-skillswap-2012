test : 
	cabal configure
	cabal build
	cp dist/build/rpc-test/guesser .

run : test
	./guesser

clean :
	rm -f -R dist
	rm guesser