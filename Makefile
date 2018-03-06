run: main
	./build/main input.scheme

main: main.hs
	ghc -odir build main.hs
