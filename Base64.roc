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
