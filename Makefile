.PHONY: build run repl test

build:
	cabal build christmas-gift-drawing
	
run: build
	cabal run christmas-gift-drawing
	
repl:
	cabal repl christmas-gift-drawing

test:
	cabal run christmas-gift-drawing-test

dev-ghcid:
	ghcid \
		--command "cabal repl christmas-gift-drawing" \
		--reverse-errors --no-title --clear --no-height-limit -W

test-ghcid:
	ghcid \
		--command "cabal repl christmas-gift-drawing-test" \
		--test "Main.main" \
		--test-message="Running tests..." \
		--reverse-errors --no-title --clear --no-height-limit -W
