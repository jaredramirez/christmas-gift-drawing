set positional-arguments

# List available commands.
default:
    @just --list

check:
    roc check src/main.roc
    roc check src/fuzz.roc

test:
    roc test src/main.roc

run *args:
    roc src/main.roc "$@"

build:
    roc build src/main.roc --output=main

fmt:
    roc fmt src

# Check the fuzz examples, then explore for the given number of seconds.
fuzz seconds="30":
    mkdir -p fuzz-out/corpus
    roc build --fuzz src/fuzz.roc --output=fuzz-out/draw
    cd fuzz-out && ./draw check && ./draw explore corpus --time={{seconds}}
