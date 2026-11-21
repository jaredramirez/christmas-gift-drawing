# Christmas Secret Santa Generator

Development requires [Nix](https://nixos.org/download/) and
[devenv 2.x](https://devenv.sh/getting-started/). The environment pins
Roc 0.1.0_RC1 and devenv modules v2.1.2; `devenv.lock` pins the Nix inputs.
The app uses the [basic-cli](https://github.com/roc-lang/basic-cli) platform
and [roc-random](https://github.com/kili-ilo/roc-random), which Roc downloads
on first use.

```sh
devenv shell
just check
just test
```

With direnv installed, `direnv allow` activates the same environment automatically.

Create `people.json` with an ordered list of people (names must be unique):

```json
[{"name":"Alice"},{"name":"Bob"},{"name":"Carol"},{"name":"Dave"},{"name":"Eve"},{"name":"Frank"},{"name":"Grace"},{"name":"Heidi"}]
```

Run a random draw, or supply a seed from 0 to 4294967295 to reproduce one:

```sh
just run
just run --seed 42
```

The same seed, people in the same order, previous draw, and dependency versions
produce the same assignments and output links. Omitting `--seed` uses fresh
randomness. Run with `--help` for usage. The program prints a link for each
person; it does not send them.

Each person gives and receives one gift in each of four categories, with four
different recipients and no self assignments. At least five people are required.
Each link also lists, for each of that person's recipients, who gives the
recipient the other three gifts.

Pass `--previous FILE` to avoid repeating a past draw. The file maps each giver
to their recipients:

```json
{"Alice":{"small1":"Bob","small2":"Carol","big":"Dave","book":"Eve"}}
```

No one gives the same gift type to the same person as before, or gives to more
than two of their previous recipients. Some previous draws leave no room for a
new one, such as the same five or six people again; the app checks counting
bounds first and explains why instead of searching forever. Past draws are
recorded in `past-gens/`.

## Layout

- `main.roc`: the command line app
- `Draw.roc`: the draw, as pure functions of the people, previous draw, and seed
- `Base64.roc`: base64 encoding for the links
- `fuzz.roc`: a [roc-fuzz](https://github.com/lukewilliamboswell/roc-fuzz)
  target that checks the draw rules. `just fuzz 30` checks its examples, then
  explores for 30 seconds.
- `roc-bugs/`: small reproductions of problems found in Roc and its libraries
