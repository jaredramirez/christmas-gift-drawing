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

roc-check:
    roc check roc/main.roc
    roc check roc/fuzz/shuffle.roc

roc-test:
    roc test roc/main.roc

roc-run *args:
    roc roc/main.roc "$@"

# Check the fuzz examples, then explore for the given number of seconds.
roc-fuzz seconds="30":
    mkdir -p roc/fuzz/out/corpus
    roc build --fuzz roc/fuzz/shuffle.roc --output=roc/fuzz/out/shuffle
    cd roc/fuzz/out && ./shuffle check && ./shuffle explore corpus --time={{seconds}}
