.PHONY: build run repl

build:
	cabal build christmas-gift-drawing
	
run: build
	cabal run christmas-gift-drawing
	
repl:
	cabal repl christmas-gift-drawing
