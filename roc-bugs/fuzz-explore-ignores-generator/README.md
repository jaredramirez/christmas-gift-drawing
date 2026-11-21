# roc-fuzz: explored inputs can bypass the generator

roc-fuzz 0.7.0, Roc 0.1.0_RC1, Linux x86-64.

`main.roc` declares `generator: Fuzz.map(Fuzz.u64, |n| n % 4)`, so every
generated value is 0 to 3, and the property crashes on anything larger.
Exploring finds a crash within a few attempts:

```sh
roc build --fuzz main.roc --output=repro
mkdir corpus
./repro explore corpus --time=30
# [roc crashed] got 25769803776, which the generator cannot produce
```

## Why

`Fuzz.decode_input` has two input forms. `Entropy` bytes go through the
generator, but `Encoded` inputs, which come from the declared `examples` and
libFuzzer's mutations of them, are decoded straight into the input type with
the binary codec:

```roc
match Binary.form(input) {
	Ok(Entropy(bytes)) => Ok(generator(Arbitrary.new(bytes)).value)
	Ok(Encoded) => Binary.decode(input).map_err(|_| InvalidInput)
	...
}
```

So any constraint expressed in the generator (ranges, `map`, `map2`) does not
hold for mutated examples. This may be intended, since the `Parseable` docs
mention decoding "mutated source-authored seeds", but nothing on `Generator`,
`map` or `target_with` says the generator can be skipped.

## Workaround

Constrain or `Fuzz.reject` inside the property instead of relying on the
generator. `fuzz.roc` in this repo reduces `extra_people` with `% 8` in the
test itself for this reason.
