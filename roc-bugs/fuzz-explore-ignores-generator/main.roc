app [target] { fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.7.0/HBigHQhVZWEF2EkLxpYK35u8P6UNsAmPYxpNFTxXK1Br.tar.zst" }

import fuzz.Fuzz

## The generator only produces values from 0 to 3, so this should never crash
test! : U64 => Fuzz.Outcome
test! = |n| {
	if n > 3 {
		crash "got ${n.to_str()}, which the generator cannot produce"
	}
	Fuzz.keep
}

target = Fuzz.target_with!({
	examples: [Fuzz.example({ name: "zero", revision: "1", make: || 0 })],
	name: "explore-ignores-generator",
	generator: Fuzz.map(Fuzz.u64, |n| n % 4),
	test!,
	show: |n| Str.inspect(n),
})
