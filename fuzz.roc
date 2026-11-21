app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.7.0/HBigHQhVZWEF2EkLxpYK35u8P6UNsAmPYxpNFTxXK1Br.tar.zst",
	rand: "https://github.com/kili-ilo/roc-random/releases/download/0.9.3/HoULZB8QzktiNPYSe9myzy9JQzm1nMbjDPVWoA63QpVf.tar.zst",
}

import fuzz.Fuzz
import Draw

## Raw fuzz input; `extra_people` is reduced to 0 to 7 people over the minimum
Input : { seed : U32, extra_people : U8 }

## People named "0", "1", ...
people_for : U64 -> List(Str)
people_for = |count| List.from_iter((0.U64).until(count)).map(|n| n.to_str())

## A draw is valid and inverts correctly, and follow-up draws with the same
## people or one more respect it as the previous draw
test! : Input => Fuzz.Outcome
test! = |{ seed, extra_people }| {
	count = Draw.min_people + (extra_people % 8).to_u64()
	people = people_for(count)
	previous = match Draw.draw(people, Dict.empty(), seed) {
		Ok(assignments) => assignments
		Err(err) => crash "draw failed: ${Str.inspect(err)}"
	}
	if !Draw.is_valid(people, previous) {
		crash "draw broke a gift rule"
	}

	givers = Draw.gift_givers(previous)
	inverts = previous.to_list().all(
		|(giver, gifts)|
			Draw.gift_types.all(|(_, gift_type)| givers.get(gift_type(gifts)).map_ok(gift_type) == Ok(giver)),
	)
	if !inverts {
		crash "gift_givers did not invert the draw"
	}

	# Same people again next year, and with one person joining
	check_follow_up!(people, previous, seed.bitwise_xor(1))
	check_follow_up!(people_for(count + 1), previous, seed.bitwise_xor(2))
	Fuzz.keep
}

## A follow-up draw is either valid or rejected by the counting bounds. The
## bounds must reject every follow-up to fewer than 7 previous people: with the
## same 6, each giver has only 1 new recipient; with a newcomer, all 6 need them
## but they receive only 4 gifts.
check_follow_up! : List(Str), Draw.Assignments, U32 => {}
check_follow_up! = |people, previous, seed| {
	previous_count = previous.len()
	match Draw.draw(people, previous, seed) {
		Ok(_) if previous_count < 7 => crash "a follow-up to ${previous_count.to_str()} people should be impossible"
		Ok(next) if Draw.is_valid(people, next) and Draw.respects_previous(previous, next) => {}
		Ok(_) => crash "follow-up draw broke a rule"
		Err(CannotAvoidPrevious(_)) | Err(NotEnoughNewPairings(_, _)) if previous_count < 7 => {}
		Err(err) => crash "follow-up from ${previous_count.to_str()} to ${List.len(people).to_str()} people failed: ${Str.inspect(err)}"
	}
}

target = Fuzz.target_with!({
	examples: [
		Fuzz.example({ name: "smallest", revision: "1", make: || { seed: 0, extra_people: 0 } }),
		Fuzz.example({ name: "family-size", revision: "1", make: || { seed: 42, extra_people: 4 } }),
	],
	name: "draw",
	generator: Fuzz.map2(Fuzz.u64, Fuzz.u8, |seed, extra_people| { seed: seed.to_u32_wrap(), extra_people }),
	test!,
	show: |input| Str.inspect(input),
})
