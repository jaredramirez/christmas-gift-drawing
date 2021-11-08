build-shell:
	cabal build christmas-gift-drawing
	
run-shell:
	cabal run christmas-gift-drawing
	
repl-shell:
	cabal repl christmas-gift-drawing

build:
	nix build .
	
run:
	./result/bin/christmas-gift-drawing
