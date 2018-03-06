run: hello
	./hello input.scheme

hello: hello.hs
	ghc hello.hs
