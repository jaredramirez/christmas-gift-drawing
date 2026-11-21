app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	rand: "https://github.com/kili-ilo/roc-random/releases/download/0.9.3/HoULZB8QzktiNPYSe9myzy9JQzm1nMbjDPVWoA63QpVf.tar.zst",
}

import pf.OsStr
import pf.Stdout
import pf.Path
import pf.Random as Seed
import rand.Random

# Spike: read people.json, shuffle it with a platform seed, and base64 encode
# a payload. The draw itself is not ported yet.

Person := { name : Str }.{
	parser_for : _
	encoder_for : _
}

alphabet : List(U8)
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".to_utf8()

## Standard padded base64
base64 : List(U8) -> Str
base64 = |bytes| {
	var $out = []
	var $i = 0
	len = List.len(bytes)
	while $i < len {
		b0 = List.get(bytes, $i) ?? 0
		b1 = List.get(bytes, $i + 1) ?? 0
		b2 = List.get(bytes, $i + 2) ?? 0
		n = b0.to_u32().shl_wrap(16).bitwise_or(b1.to_u32().shl_wrap(8)).bitwise_or(b2.to_u32())
		char = |shift| List.get(alphabet, n.shr_zf_wrap(shift).bitwise_and(63).to_u64()) ?? '='
		remaining = len - $i
		$out = List.concat($out, [
			char(18),
			char(12),
			if remaining > 1 char(6) else '=',
			if remaining > 2 char(0) else '=',
		])
		$i = $i + 3
	}
	Str.from_utf8($out) ?? ""
}

main! : List(OsStr) => Try({}, _)
main! = |_args| {
	people_path : Path
	people_path = "people.json"
	json = people_path.read_utf8!()?
	people : List(Person)
	people = Json.parse(json)?

	n = Seed.seed_u32!()?
	shuffled = Random.step(Random.seed(n), Random.shuffle(people)).value

	Stdout.line!("Read and shuffled ${List.len(shuffled).to_str()} people")?
	Ok({})
}

expect {
	{ value, .. } = Random.step(Random.seed(42), Random.shuffle([1, 2, 3, 4, 5]))
	value.sort() == [1, 2, 3, 4, 5]
}

# RFC 4648 section 10 test vectors
expect base64("".to_utf8()) == ""
expect base64("f".to_utf8()) == "Zg=="
expect base64("fo".to_utf8()) == "Zm8="
expect base64("foo".to_utf8()) == "Zm9v"
expect base64("foob".to_utf8()) == "Zm9vYg=="
expect base64("fooba".to_utf8()) == "Zm9vYmE="
expect base64("foobar".to_utf8()) == "Zm9vYmFy"

# Non-UTF-8 bytes, checked against `base64 -w0`
expect base64([0, 255, 128, 63, 191]) == "AP+AP78="
expect base64([251, 255, 254]) == "+//+"
