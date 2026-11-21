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

Then run the app:

```sh
just run
```

The app currently reads `people.json` and shuffles it; the draw itself is not
implemented yet. Each person will give and receive one gift in each of four
categories, with four different recipients and no self assignments. Past draws
are recorded in `past-gens/`.

`just fuzz 30` builds the [roc-fuzz](https://github.com/lukewilliamboswell/roc-fuzz)
targets in `fuzz/`, checks their examples, then explores for 30 seconds.
