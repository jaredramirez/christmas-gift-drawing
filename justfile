set positional-arguments

# List available commands.
default:
    @just --list

build:
    cabal build all

run *args:
    cabal run christmas-gift-drawing-exe -- "$@"

repl:
    cabal repl christmas-gift-drawing

test:
    cabal test all

dev:
    ghciwatch \
        --command "cabal repl christmas-gift-drawing" \
        --watch src --watch christmas-gift-drawing.cabal --clear

test-watch:
    ghciwatch \
        --command "cabal repl christmas-gift-drawing-test" \
        --watch src --watch test --watch christmas-gift-drawing.cabal \
        --restart-glob 'src/**/*.hs' \
        --test-ghci "Main.main" --clear
