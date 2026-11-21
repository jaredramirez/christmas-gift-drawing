app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.7.0/HBigHQhVZWEF2EkLxpYK35u8P6UNsAmPYxpNFTxXK1Br.tar.zst",
	rand: "https://github.com/kili-ilo/roc-random/releases/download/0.9.3/HoULZB8QzktiNPYSe9myzy9JQzm1nMbjDPVWoA63QpVf.tar.zst",
}

import fuzz.Fuzz
import rand.Random

## Shuffling must keep exactly the same elements
test! : List(U8) => Fuzz.Outcome
test! = |input| {
	seed = Random.seed(List.len(input).to_u32_wrap())
	shuffled = Random.step(seed, Random.shuffle(input)).value
	if shuffled.sort() == input.sort() {
		Fuzz.keep
	} else {
		crash "shuffle changed the elements"
	}
}

target = Fuzz.target_with!({
	examples: [
		Fuzz.example({ name: "empty", revision: "1", make: || [] }),
		Fuzz.example({ name: "representative", revision: "1", make: || [3, 1, 2, 2] }),
	],
	name: "shuffle-is-permutation",
	generator: Fuzz.bytes,
	test!,
	show: |input| Str.inspect(input),
})
