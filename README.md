# Christmas Secret Santa Generator

Development requires [Nix](https://nixos.org/download/) and
[devenv 2.x](https://devenv.sh/getting-started/). The environment pins GHC 9.14.1
and devenv modules v2.1.2; `devenv.lock` pins the Nix inputs.

```sh
devenv shell
cabal update
just build
just test
```

With direnv installed, `direnv allow` activates the same environment automatically.
Haskell Language Server 2.15.0.0 comes from a separate, locked `haskell-updates`
Nixpkgs input, using the same GHC as the project. Nix uses its binary cache plus
the HLS and nix-community caches where builds are available; missing packages
are built locally once and reused. `ghciwatch`, `fourmolu`, and `just` are included.

For automatic reloads, use `just dev`; `just test-watch` reloads test edits and
restarts GHCi on library edits so Cabal rebuilds the library before rerunning tests.
These use the
[ghciwatch CLI](https://mercurytechnologies.github.io/ghciwatch/cli.html).

Create `people.json` with an ordered list of people (names must be unique):

```json
[{"name":"Alice"},{"name":"Bob"},{"name":"Carol"},{"name":"Dave"},{"name":"Eve"},{"name":"Frank"},{"name":"Grace"},{"name":"Heidi"}]
```

Run a random draw, or supply an integer seed to reproduce one:

```sh
cabal run christmas-gift-drawing-exe
cabal run christmas-gift-drawing-exe -- --seed 42
# Equivalent just command:
just run --seed 42
```

The same seed, people in the same order, and dependency versions produce the same
assignments and output links. Omitting `--seed` uses fresh randomness. Run with
`--help` for usage. The program prints links; it does not send emails.

Each person gives and receives one gift in each of four categories, with four
different recipients and no self assignments. At least five people are required.
The existing pairing algorithm retries random cycles, so a draw can take a long
time or get stuck if earlier categories leave no valid cycle for a later one.
