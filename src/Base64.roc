## Standard padded base64 (RFC 4648)
Base64 := [].{
	alphabet : List(U8)
	alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".to_utf8()

	encode : List(U8) -> Try(Str, [OutOfBounds, BadUtf8(_)])
	encode = |bytes| {
		chars = encode_chunks(bytes, [])?
		Str.from_utf8(chars)
	}

	## Encode three bytes at a time, padding the last one or two
	encode_chunks : List(U8), List(U8) -> Try(List(U8), [OutOfBounds])
	encode_chunks = |bytes, out|
		match bytes {
			[] => Ok(out)
			[b0] => {
				n = join(b0, 0, 0)
				Ok(out.concat([char(n, 18)?, char(n, 12)?, '=', '=']))
			}
			[b0, b1] => {
				n = join(b0, b1, 0)
				Ok(out.concat([char(n, 18)?, char(n, 12)?, char(n, 6)?, '=']))
			}
			[b0, b1, b2, .. as rest] => {
				n = join(b0, b1, b2)
				encode_chunks(rest, out.concat([char(n, 18)?, char(n, 12)?, char(n, 6)?, char(n, 0)?]))
			}
		}

	## Three bytes as one 24-bit number
	join : U8, U8, U8 -> U32
	join = |b0, b1, b2| b0.to_u32().shl_wrap(16).bitwise_or(b1.to_u32().shl_wrap(8)).bitwise_or(b2.to_u32())

	## The character for the six bits of `n` starting at `shift`
	char : U32, U8 -> Try(U8, [OutOfBounds])
	char = |n, shift| List.get(alphabet, n.shr_zf_wrap(shift).bitwise_and(63).to_u64())

	decode : Str -> Try(List(U8), [InvalidBase64])
	decode = |text| decode_chunks(text.to_utf8(), [])

	## Decode four characters at a time; only the last group may end in padding
	decode_chunks : List(U8), List(U8) -> Try(List(U8), [InvalidBase64])
	decode_chunks = |chars, out|
		match chars {
			[] => Ok(out)
			[c0, c1, '=', '='] => {
				n = split(c0, c1, 'A', 'A')?
				Ok(out.append(byte(n, 16)))
			}
			[c0, c1, c2, '='] => {
				n = split(c0, c1, c2, 'A')?
				Ok(out.concat([byte(n, 16), byte(n, 8)]))
			}
			[c0, c1, c2, c3, .. as rest] => {
				n = split(c0, c1, c2, c3)?
				decode_chunks(rest, out.concat([byte(n, 16), byte(n, 8), byte(n, 0)]))
			}
			[_] | [_, _] | [_, _, _] => Err(InvalidBase64)
		}

	## Four characters as one 24-bit number
	split : U8, U8, U8, U8 -> Try(U32, [InvalidBase64])
	split = |c0, c1, c2, c3| {
		v0 = value(c0)?
		v1 = value(c1)?
		v2 = value(c2)?
		v3 = value(c3)?
		Ok(v0.shl_wrap(18).bitwise_or(v1.shl_wrap(12)).bitwise_or(v2.shl_wrap(6)).bitwise_or(v3))
	}

	## The eight bits of `n` starting at `shift`
	byte : U32, U8 -> U8
	byte = |n, shift| U32.to_u8_wrap(n.shr_zf_wrap(shift))

	## The six bits a character stands for
	value : U8 -> Try(U32, [InvalidBase64])
	value = |c|
		if c >= 'A' and c <= 'Z' {
			Ok((c - 'A').to_u32())
		} else if c >= 'a' and c <= 'z' {
			Ok((c - 'a').to_u32() + 26)
		} else if c >= '0' and c <= '9' {
			Ok((c - '0').to_u32() + 52)
		} else if c == '+' {
			Ok(62)
		} else if c == '/' {
			Ok(63)
		} else {
			Err(InvalidBase64)
		}
}

# RFC 4648 section 10 test vectors
expect Base64.encode("".to_utf8()) == Ok("")
expect Base64.encode("f".to_utf8()) == Ok("Zg==")
expect Base64.encode("fo".to_utf8()) == Ok("Zm8=")
expect Base64.encode("foo".to_utf8()) == Ok("Zm9v")
expect Base64.encode("foob".to_utf8()) == Ok("Zm9vYg==")
expect Base64.encode("fooba".to_utf8()) == Ok("Zm9vYmE=")
expect Base64.encode("foobar".to_utf8()) == Ok("Zm9vYmFy")

# Non-UTF-8 bytes, checked against `base64 -w0` and `openssl base64 -A`
expect Base64.encode([0, 255, 128, 63, 191]) == Ok("AP+AP78=")
expect Base64.encode([251, 255, 254]) == Ok("+//+")

# RFC 4648 section 10 test vectors, decoded
expect Base64.decode("") == Ok("".to_utf8())
expect Base64.decode("Zg==") == Ok("f".to_utf8())
expect Base64.decode("Zm8=") == Ok("fo".to_utf8())
expect Base64.decode("Zm9v") == Ok("foo".to_utf8())
expect Base64.decode("Zm9vYg==") == Ok("foob".to_utf8())
expect Base64.decode("Zm9vYmE=") == Ok("fooba".to_utf8())
expect Base64.decode("Zm9vYmFy") == Ok("foobar".to_utf8())

# The non-UTF-8 vectors above, in reverse
expect Base64.decode("AP+AP78=") == Ok([0, 255, 128, 63, 191])
expect Base64.decode("+//+") == Ok([251, 255, 254])

# Every byte value survives a round trip
expect {
	bytes = List.from_iter((0.U32).until(256)).map(U32.to_u8_wrap)
	match Base64.encode(bytes) {
		Ok(text) => Base64.decode(text) == Ok(bytes)
		Err(_) => False
	}
}

# Wrong lengths, characters outside the alphabet, and misplaced padding
expect Base64.decode("Z") == Err(InvalidBase64)
expect Base64.decode("Zg") == Err(InvalidBase64)
expect Base64.decode("Zg=") == Err(InvalidBase64)
expect Base64.decode("Zm9vY") == Err(InvalidBase64)
expect Base64.decode("Zm9v!A==") == Err(InvalidBase64)
expect Base64.decode("Zm 9") == Err(InvalidBase64)
expect Base64.decode("Zm9-") == Err(InvalidBase64)
expect Base64.decode("====") == Err(InvalidBase64)
expect Base64.decode("Z===") == Err(InvalidBase64)
expect Base64.decode("Zg=a") == Err(InvalidBase64)
expect Base64.decode("=Zm9") == Err(InvalidBase64)
expect Base64.decode("Zg==Zg==") == Err(InvalidBase64)
