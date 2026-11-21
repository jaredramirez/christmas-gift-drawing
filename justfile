set positional-arguments

# List available commands.
default:
    @just --list

check:
    roc check main.roc
    roc check fuzz/shuffle.roc

test:
    roc test main.roc

run *args:
    roc main.roc "$@"

build:
    roc build main.roc

fmt:
    roc fmt main.roc fuzz

# Check the fuzz examples, then explore for the given number of seconds.
fuzz seconds="30":
    mkdir -p fuzz/out/corpus
    roc build --fuzz fuzz/shuffle.roc --output=fuzz/out/shuffle
    cd fuzz/out && ./shuffle check && ./shuffle explore corpus --time={{seconds}}
