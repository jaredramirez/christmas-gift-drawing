import rand.Random

## Gift assignment: each person gives and receives one gift of each type, with
## four different recipients and no one giving to themselves.
Draw := [].{

	## One person of each gift type
	Gifts(p) : { small1 : p, small2 : p, big : p, book : p }

	## For each giver, who they give each gift type to
	Assignments : Dict(Str, Gifts(Str))

	DrawErr : [
		TooFewPeople(U64),
		DuplicateNames(List(Str)),
		UnknownPreviousNames(List(Str)),
		CannotAvoidPrevious(List(Str)),
		NotEnoughNewPairings(U64, U64),
		GaveUp,
	]

	## The fewest people that can each give to four different others
	min_people : U64
	min_people = 5

	## The most people anyone may give to again from the previous draw
	max_repeat_recipients : U64
	max_repeat_recipients = 2

	gift_types : List((Str, (Gifts(Str) -> Str)))
	gift_types = [
		("small1", |gifts| gifts.small1),
		("small2", |gifts| gifts.small2),
		("big", |gifts| gifts.big),
		("book", |gifts| gifts.book),
	]

	recipients : Gifts(p) -> List(p)
	recipients = |gifts| [gifts.small1, gifts.small2, gifts.big, gifts.book]

	## Pair each item with its neighbor, and the last item with the first
	##
	##     make_pairs([1, 2, 3]) == [(1, 2), (2, 3), (3, 1)]
	make_pairs : List(a) -> List((a, a))
	make_pairs = |list|
		match list {
			[] => []
			[first, ..] => List.map2(list, list.drop_first(1).append(first), |a, b| (a, b))
		}

	## Draw gifts for the people, so that no one gives the same gift type to the
	## same person as in the previous draw, and no one gives to more than
	## `max_repeat_recipients` of their previous recipients.
	##
	## The same seed, people in the same order, and previous draw give the same
	## assignments.
	draw : List(Str), Assignments, U32 -> Try(Assignments, DrawErr)
	draw = |people, previous, seed| {
		count = List.len(people)
		if count < min_people {
			return Err(TooFewPeople(count))
		}

		var $duplicates = []
		for name in people {
			if people.count_if(|other| other == name) > 1 and !$duplicates.contains(name) {
				$duplicates = $duplicates.append(name)
			}
		}
		duplicates = $duplicates
		if !duplicates.is_empty() {
			return Err(DuplicateNames(duplicates))
		}

		previous_names = previous.to_list().join_map(|(giver, gifts)| [giver].concat(recipients(gifts)))
		unknown = previous_names.keep_if(|name| !people.contains(name))
		if !unknown.is_empty() {
			return Err(UnknownPreviousNames(unknown))
		}

		check_previous(people, previous)?
		draw_valid(people, previous, Random.seed(seed), 1000)
	}

	## Reject previous draws that leave no room for a new one, using counting
	## bounds every valid draw must meet. Passing them does not guarantee a draw
	## exists: each gift type must be one cycle through everyone, and deciding
	## that exactly is the Hamiltonian cycle problem.
	check_previous : List(Str), Assignments -> Try({}, [CannotAvoidPrevious(List(Str)), NotEnoughNewPairings(U64, U64)])
	check_previous = |people, previous| {
		gifts_each = List.len(gift_types)
		needed_per_giver = gifts_each - max_repeat_recipients
		new_recipients = |giver|
			match previous.get(giver) {
				Ok(gifts) => people.keep_if(|person| person != giver and !recipients(gifts).contains(person))
				Err(_) => people.keep_if(|person| person != giver)
			}
		givers_with_previous = people.keep_if(|giver| previous.contains(giver))

		# Each giver needs enough people they did not give to last time
		stuck = givers_with_previous.keep_if(|giver| List.len(new_recipients(giver)) < needed_per_giver)
		if !stuck.is_empty() {
			return Err(CannotAvoidPrevious(stuck))
		}

		# Those new pairings must fit in the gifts each person receives
		needed = List.len(givers_with_previous) * needed_per_giver
		room = people
			.map(
				|recipient| {
					givers = givers_with_previous.count_if(|giver| new_recipients(giver).contains(recipient))
					givers.min(gifts_each)
				},
			)
			.sum()
		if room < needed {
			return Err(NotEnoughNewPairings(needed, room))
		}
		Ok({})
	}

	## Draw until one is within the repeat limit, or give up
	draw_valid : List(Str), Assignments, Random.State, U64 -> Try(Assignments, DrawErr)
	draw_valid = |people, previous, seed, var $attempts_left| {
		var $seed = seed
		while $attempts_left > 0 {
			attempt = draw_once(people, previous, $seed)
			$seed = attempt.state
			match attempt.value {
				Ok(assignments) if within_repeat_limit(previous, assignments) => {
					return Ok(assignments)
				}
				_ => {}
			}
			$attempts_left = $attempts_left - 1
		}
		Err(GaveUp)
	}

	## Pair up each gift type in turn. Earlier gift types can leave no valid
	## pairing for a later one, so this can fail.
	draw_once : List(Str), Assignments, Random.State -> { value : Try(Assignments, [GaveUp]), state : Random.State }
	draw_once = |people, previous, seed| {
		previous_pairs = |gift_type| previous.to_list().map(|(giver, gifts)| (giver, gift_type(gifts)))

		small1 = random_pairs(people, previous_pairs(|g| g.small1), seed)
		small1_pairs = match small1.value {
			Ok(pairs) => pairs
			Err(GaveUp) => return { value: Err(GaveUp), state: small1.state }
		}

		small2 = random_pairs(people, previous_pairs(|g| g.small2).concat(small1_pairs), small1.state)
		small2_pairs = match small2.value {
			Ok(pairs) => pairs
			Err(GaveUp) => return { value: Err(GaveUp), state: small2.state }
		}

		big = random_pairs(people, previous_pairs(|g| g.big).concat(small1_pairs).concat(small2_pairs), small2.state)
		big_pairs = match big.value {
			Ok(pairs) => pairs
			Err(GaveUp) => return { value: Err(GaveUp), state: big.state }
		}

		book = random_pairs(
			people,
			previous_pairs(|g| g.book).concat(small1_pairs).concat(small2_pairs).concat(big_pairs),
			big.state,
		)
		book_pairs = match book.value {
			Ok(pairs) => pairs
			Err(GaveUp) => return { value: Err(GaveUp), state: book.state }
		}

		recipient_of = |pairs, giver| pairs.find_first(|(g, _)| g == giver).map_ok(|(_, r)| r) ?? giver
		assignments = Dict.from_list(
			people.map(
				|giver| (
					giver,
					{
						small1: recipient_of(small1_pairs, giver),
						small2: recipient_of(small2_pairs, giver),
						big: recipient_of(big_pairs, giver),
						book: recipient_of(book_pairs, giver),
					},
				),
			),
		)
		{ value: Ok(assignments), state: book.state }
	}

	## Shuffle the people into a cycle, retrying until no pair is excluded
	random_pairs : List(Str), List((Str, Str)), Random.State -> { value : Try(List((Str, Str)), [GaveUp]), state : Random.State }
	random_pairs = |people, excluded, seed| {
		var $seed = seed
		var $attempts_left = 10_000
		while $attempts_left > 0 {
			shuffled = Random.step($seed, Random.shuffle(people))
			$seed = shuffled.state
			pairs = make_pairs(shuffled.value)
			if pairs.all(|pair| !excluded.contains(pair)) {
				return { value: Ok(pairs), state: $seed }
			}
			$attempts_left = $attempts_left - 1
		}
		{ value: Err(GaveUp), state: $seed }
	}

	within_repeat_limit : Assignments, Assignments -> Bool
	within_repeat_limit = |previous, assignments|
		assignments.to_list().all(
			|(giver, gifts)|
				match previous.get(giver) {
					Ok(previous_gifts) => {
						current = recipients(gifts)
						repeats = recipients(previous_gifts).count_if(|name| current.contains(name))
						repeats <= max_repeat_recipients
					}
					Err(_) => True
				},
		)

	## Invert a draw: for each person, who gives them each gift type
	gift_givers : Assignments -> Assignments
	gift_givers = |assignments| {
		givers = assignments.to_list()
		giver_of = |recipient, gift_type|
			givers.find_first(|(_, gifts)| gift_type(gifts) == recipient).map_ok(|(giver, _)| giver) ?? recipient
		Dict.from_list(
			givers.map(
				|(person, _)| (
					person,
					{
						small1: giver_of(person, |g| g.small1),
						small2: giver_of(person, |g| g.small2),
						big: giver_of(person, |g| g.big),
						book: giver_of(person, |g| g.book),
					},
				),
			),
		)
	}

	## For each of a giver's recipients, who gives that recipient the other gift
	## types, keyed by gift type
	other_givers : Assignments, Gifts(Str) -> Dict(Str, Dict(Str, Str))
	other_givers = |givers, gifts|
		Dict.from_list(
			gift_types.map(
				|(gift_type, to_recipient)| {
					recipient = to_recipient(gifts)
					others = match givers.get(recipient) {
						Ok(recipient_givers) =>
							gift_types
								.keep_if(|(other_type, _)| other_type != gift_type)
								.map(|(other_type, to_giver)| (other_type, to_giver(recipient_givers)))
						Err(_) => []
					}
					(gift_type, Dict.from_list(others))
				},
			),
		)

	## Every gift type is a full cycle, and everyone has four different
	## recipients, none of them themselves
	is_valid : List(Str), Assignments -> Bool
	is_valid = |people, assignments| {
		gives_once = gift_types.all(
			|(_, gift_type)| {
				received = assignments.to_list().map(|(_, gifts)| gift_type(gifts))
				people.all(|person| received.count_if(|name| name == person) == 1)
			},
		)
		distinct = assignments.to_list().all(
			|(giver, gifts)| {
				names = recipients(gifts)
				!names.contains(giver) and names.all(|name| names.count_if(|other| other == name) == 1)
			},
		)
		assignments.keys() == people and gives_once and distinct
	}

	## No one gives the same gift type to the same person as in the previous
	## draw, or gives to more than `max_repeat_recipients` previous recipients
	respects_previous : Assignments, Assignments -> Bool
	respects_previous = |previous, assignments| {
		no_same_gift = assignments.to_list().all(
			|(giver, gifts)|
				match previous.get(giver) {
					Ok(previous_gifts) => gift_types.all(|(_, gift_type)| gift_type(gifts) != gift_type(previous_gifts))
					Err(_) => True
				},
		)
		no_same_gift and within_repeat_limit(previous, assignments)
	}
}

test_people : List(Str)
test_people = ["1", "2", "3", "4", "5", "6", "7", "8"]

# 5 people give to all 4 others, so the same people cannot avoid repeating
expect {
	five = ["1", "2", "3", "4", "5"]
	match Draw.draw(five, Dict.empty(), 0) {
		Ok(previous) => Draw.draw(five, previous, 1) == Err(CannotAvoidPrevious(five))
		Err(_) => False
	}
}

# 6 previous givers each need the one newcomer, who receives only 4 gifts
expect {
	six = ["1", "2", "3", "4", "5", "6"]
	match Draw.draw(six, Dict.empty(), 0) {
		Ok(previous) => Draw.draw(six.append("7"), previous, 1) == Err(NotEnoughNewPairings(12, 10))
		Err(_) => False
	}
}

expect Draw.make_pairs([1, 2, 3]) == [(1, 2), (2, 3), (3, 1)]
expect Draw.make_pairs([]) == []

expect {
	first = Draw.draw(test_people, Dict.empty(), 42)
	first == Draw.draw(test_people, Dict.empty(), 42) and first != Draw.draw(test_people, Dict.empty(), 123)
}

expect List.from_iter((0.U32).until(20)).all(
	|seed|
		match Draw.draw(test_people, Dict.empty(), seed) {
			Ok(assignments) => Draw.is_valid(test_people, assignments)
			Err(_) => False
		},
)

expect Draw.draw(["1", "2", "3", "4"], Dict.empty(), 0) == Err(TooFewPeople(4))
expect Draw.draw(["1", "2", "3", "4", "1"], Dict.empty(), 0) == Err(DuplicateNames(["1"]))

expect {
	previous = Dict.from_list([("9", { small1: "1", small2: "2", big: "3", book: "4" })])
	Draw.draw(test_people, previous, 0) == Err(UnknownPreviousNames(["9"]))
}

expect {
	more_people = test_people.append("9")
	match Draw.draw(test_people, Dict.empty(), 0) {
		Ok(previous) =>
			List.from_iter((1.U32).to(5)).all(
				|seed|
					match Draw.draw(more_people, previous, seed) {
						Ok(assignments) => Draw.is_valid(more_people, assignments) and Draw.respects_previous(previous, assignments)
						Err(_) => False
					},
			)
		Err(_) => False
	}
}

expect
	match Draw.draw(test_people, Dict.empty(), 7) {
		Ok(assignments) => {
			givers = Draw.gift_givers(assignments)
			assignments.to_list().all(
				|(giver, gifts)|
					Draw.gift_types.all(
						|(_, gift_type)|
							match givers.get(gift_type(gifts)) {
								Ok(recipient_givers) => gift_type(recipient_givers) == giver
								Err(_) => False
							},
					),
			)
		}
		Err(_) => False
	}

expect
	match Draw.draw(test_people, Dict.empty(), 7) {
		Ok(assignments) => {
			givers = Draw.gift_givers(assignments)
			assignments.to_list().all(
				|(giver, gifts)| {
					others = Draw.other_givers(givers, gifts)
					Draw.gift_types.all(
						|(gift_type, to_recipient)| {
							recipient = to_recipient(gifts)
							match others.get(gift_type) {
								Ok(by_type) =>
									by_type.len() == 3
										and !by_type.contains(gift_type)
										and by_type.to_list().all(
											|(other_type, other_giver)|
												other_giver != giver
													and assignments.get(other_giver).map_ok(|g| Draw.recipients(g).contains(recipient)) == Ok(True)
													and other_type != gift_type,
										)
								Err(_) => False
							}
						},
					)
				},
			)
		}
		Err(_) => False
	}
