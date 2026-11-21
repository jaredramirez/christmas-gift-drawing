app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	rand: "https://github.com/kili-ilo/roc-random/releases/download/0.9.3/HoULZB8QzktiNPYSe9myzy9JQzm1nMbjDPVWoA63QpVf.tar.zst",
}

import pf.OsStr
import pf.Path
import pf.Random as PlatformRandom
import pf.Stderr
import pf.Stdout
import Base64
import Draw

base_url : Str
base_url = "https://2026.secret-santa.jaredramirez.omg.lol/?data="

usage : Str
usage =
	\\Draw gift assignments from people.json and print a link for each person.
	\\
	\\Usage: main.roc [--seed INTEGER] [--previous FILE]
	\\
	\\  --seed INTEGER   Reproduce a draw with a seed from 0 to 4294967295
	\\  --previous FILE  A previous draw to avoid repeating, as JSON mapping each
	\\                   giver to their small1, small2, big, and book recipients
	\\  --help           Show this message

Args : {
	seed : [Fixed(U32), Fresh],
	previous : [File(Str), NoPrevious],
	help : Bool,
}

parse_args : List(Str), Args -> Try(Args, [BadArgs(Str)])
parse_args = |args, parsed|
	match args {
		[] => Ok(parsed)
		["--help", .. as rest] => parse_args(rest, { ..parsed, help: True })
		["--seed", value, .. as rest] =>
			match U32.from_str(value) {
				Ok(seed) => parse_args(rest, { ..parsed, seed: Fixed(seed) })
				Err(_) => Err(BadArgs("--seed must be an integer from 0 to 4294967295, but got ${value}"))
			}
		["--previous", path, .. as rest] => parse_args(rest, { ..parsed, previous: File(path) })
		[other, ..] => Err(BadArgs("Unknown or incomplete argument: ${other}"))
	}

## Read and parse a JSON file
read_json! : Str => Try(a, [ReadFailed(Str), ParseFailed(Str, Str)]) where [a.Json.Parseable([InvalidJson(Str)])]
read_json! = |name| {
	path : Path
	path = Path.from_os_str(OsStr.from_str(name))
	contents = path.read_utf8!() ? |_| ReadFailed(name)
	Json.parse(contents).map_err(|InvalidJson(msg)| ParseFailed(name, msg))
}

run! = |os_args| {
	args = parse_args(os_args.map(OsStr.display), { seed: Fresh, previous: NoPrevious, help: False })?
	if args.help {
		Stdout.line!(usage)?
		return Ok({})
	}

	people : List({ name : Str })
	people = read_json!("people.json")?
	names = people.map(|person| person.name)

	previous : Draw.Assignments
	previous = match args.previous {
		File(path) => read_json!(path)?
		NoPrevious => Dict.empty()
	}

	seed = match args.seed {
		Fixed(value) => value
		Fresh => PlatformRandom.seed_u32!()?
	}

	assignments = Draw.draw(names, previous, seed)?
	givers = Draw.gift_givers(assignments)

	# Assignments keep the order of people.json
	for (name, gifts) in assignments.to_list() {
		payload = {
			recipient: name,
			small1: gifts.small1,
			small2: gifts.small2,
			big: gifts.big,
			book: gifts.book,
			other_givers: Draw.other_givers(givers, gifts),
		}
		Stdout.line!("${name} : ${base_url}${Base64.encode(Json.to_str(payload).to_utf8())}")?
	}
	Ok({})
}

error_message = |err|
	match err {
		BadArgs(msg) => "${msg}\n\n${usage}"
		ReadFailed(name) => "Could not read ${name}"
		ParseFailed(name, msg) => "Could not parse ${name}: ${msg}"
		TooFewPeople(count) => "At least ${Draw.min_people.to_str()} people are needed, but there are ${count.to_str()}"
		DuplicateNames(names) => "Names must be unique, but these repeat: ${Str.join_with(names, ", ")}"
		UnknownPreviousNames(names) => "Names in the previous draw are not in people.json: ${Str.join_with(names, ", ")}"
		CannotAvoidPrevious(names) =>
			"No draw can avoid the previous one: these people have fewer than 2 others they did not give to last time: ${Str.join_with(names, ", ")}"
		NotEnoughNewPairings(needed, room) =>
			"No draw can avoid the previous one: it needs ${needed.to_str()} new pairings, but there is room for only ${room.to_str()}"
		GaveUp => "Could not find a draw that avoids the previous one; try another seed"
		other => "Unexpected error: ${Str.inspect(other)}"
	}

main! : List(OsStr) => Try({}, _)
main! = |args|
	match run!(args) {
		Ok({}) => Ok({})
		Err(err) => {
			Stderr.line!(error_message(err))?
			Err(Exit(1))
		}
	}
