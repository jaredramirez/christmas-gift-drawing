set positional-arguments

# List available commands.
default:
    @just --list

check:
    roc check main.roc
    roc check fuzz.roc

test:
    roc test main.roc

run *args:
    roc main.roc "$@"

build:
    roc build main.roc

fmt:
    roc fmt main.roc Draw.roc Base64.roc fuzz.roc

# Check the fuzz examples, then explore for the given number of seconds.
fuzz seconds="30":
    mkdir -p fuzz-out/corpus
    roc build --fuzz fuzz.roc --output=fuzz-out/draw
    cd fuzz-out && ./draw check && ./draw explore corpus --time={{seconds}}
