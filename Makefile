.PHONY: build run repl test

build:
	cabal build christmas-gift-drawing
	
run: build
	cabal run christmas-gift-drawing-exe
	
repl:
	cabal repl christmas-gift-drawing

test:
	cabal run christmas-gift-drawing-test

test-ghcid:
	ghcid \
		--command "cabal repl christmas-gift-drawing-test christmas-gift-drawing --repl-options=-ferror-spans --repl-options=-fdiagnostics-color=always --repl-options=-Wwarn" \
		--test "Spec.main" \
		--test-message="Running tests..." \
		--reverse-errors --no-title --clear --no-height-limit

exe-ghcid:
	ghcid \
		--command "cabal repl christmas-gift-drawing-exe christmas-gift-drawing --repl-options=-ferror-spans --repl-options=-fdiagnostics-color=always --repl-options=-Wwarn" \
		--reverse-errors --no-title --clear --no-height-limit
