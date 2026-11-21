## Standard padded base64 (RFC 4648)
Base64 := [].{
	alphabet : List(U8)
	alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".to_utf8()

	encode : List(U8) -> Str
	encode = |bytes| {
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
			$out = List.concat(
				$out,
				[
					char(18),
					char(12),
					if (remaining > 1) char(6) else '=',
					if (remaining > 2) char(0) else '=',
				],
			)
			$i = $i + 3
		}
		Str.from_utf8($out) ?? ""
	}
}

# RFC 4648 section 10 test vectors
expect Base64.encode("".to_utf8()) == ""
expect Base64.encode("f".to_utf8()) == "Zg=="
expect Base64.encode("fo".to_utf8()) == "Zm8="
expect Base64.encode("foo".to_utf8()) == "Zm9v"
expect Base64.encode("foob".to_utf8()) == "Zm9vYg=="
expect Base64.encode("fooba".to_utf8()) == "Zm9vYmE="
expect Base64.encode("foobar".to_utf8()) == "Zm9vYmFy"

# Non-UTF-8 bytes, checked against `base64 -w0` and `openssl base64 -A`
expect Base64.encode([0, 255, 128, 63, 191]) == "AP+AP78="
expect Base64.encode([251, 255, 254]) == "+//+"
