app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	rand: "https://github.com/kili-ilo/roc-random/releases/download/0.9.3/HoULZB8QzktiNPYSe9myzy9JQzm1nMbjDPVWoA63QpVf.tar.zst",
}

import pf.Cmd
import pf.OsStr
import pf.Path
import pf.Sleep
import pf.Stderr
import pf.Stdout
import Base64
import Draw

base_url : Str
base_url = "https://2026.secret-santa.jaredramirez.omg.lol/?data="

gens_file : Str
gens_file = "past-gens/2026.txt"

messages_file : Str
messages_file = "follow-up.jsonl"

phones_file : Str
phones_file = "phones.json"

sent_file : Str
sent_file = "sent.log"

## Milliseconds to wait between texts
send_pause : U64
send_pause = 2000

usage : Str
usage =
	\\Send each person a follow-up link that also lists who else gives to their recipients.
	\\
	\\Usage: follow-up.roc COMMAND
	\\
	\\  prepare                  Rebuild the draw from past-gens/2026.txt, check it, and
	\\                           write the new messages to follow-up.jsonl
	\\  contacts [--as NAME CONTACT]...
	\\                           Look up each person's phone number in Contacts and save
	\\                           it to phones.json. Entries already there are kept. Use
	\\                           --as to search Contacts for a different name, such as
	\\                           --as Mom "Jane Doe"
	\\  send [--send] [--only NAME]
	\\                           List who would get a text. With --send, text them over
	\\                           iMessage, recording each name in sent.log so a rerun
	\\                           skips it. With --only, just that person
	\\  --help                   Show this message

Command : [Prepare, Contacts(Dict(Str, Str)), Send({ send : Bool, only : [Only(Str), Everyone] }), Help]

parse_args : List(Str) -> Try(Command, [BadArgs(Str)])
parse_args = |args|
	match args {
		["prepare"] => Ok(Prepare)
		["contacts", .. as rest] => parse_aliases(rest, Dict.empty()).map_ok(|aliases| Contacts(aliases))
		["send", .. as rest] => parse_send(rest, { send: False, only: Everyone }).map_ok(|options| Send(options))
		["--help"] | [] => Ok(Help)
		[other, ..] => Err(BadArgs("Unknown command: ${other}"))
	}

parse_aliases : List(Str), Dict(Str, Str) -> Try(Dict(Str, Str), [BadArgs(Str)])
parse_aliases = |args, aliases|
	match args {
		[] => Ok(aliases)
		["--as", name, contact, .. as rest] => parse_aliases(rest, aliases.insert(name, contact))
		[other, ..] => Err(BadArgs("Unknown or incomplete argument to contacts: ${other}"))
	}

parse_send : List(Str), { send : Bool, only : [Only(Str), Everyone] } -> Try({ send : Bool, only : [Only(Str), Everyone] }, [BadArgs(Str)])
parse_send = |args, parsed|
	match args {
		[] => Ok(parsed)
		["--send", .. as rest] => parse_send(rest, { ..parsed, send: True })
		["--only", name, .. as rest] => parse_send(rest, { ..parsed, only: Only(name) })
		[other, ..] => Err(BadArgs("Unknown or incomplete argument to send: ${other}"))
	}

path : Str -> Path
path = |name| Path.from_os_str(OsStr.from_str(name))

read! : Str => Try(Str, [ReadFailed(Str)])
read! = |name| path(name).read_utf8!().map_err(|_| ReadFailed(name))

## A file's contents, or "" when it does not exist yet
read_optional! : Str => Try(Str, [ReadFailed(Str)])
read_optional! = |name| {
	exists = path(name).exists!() ? |_| ReadFailed(name)
	if (exists) read!(name) else Ok("")
}

write! : Str, Str => Try({}, [WriteFailed(Str)])
write! = |name, content| path(name).write_utf8!(content).map_err(|_| WriteFailed(name))

## The non-blank lines of a file
lines : Str -> List(Str)
lines = |contents| contents.split_on("\n").map(Str.trim).keep_if(|line| !line.is_empty())

## ---- prepare ----

## A 2026 link's payload. `recipient` is the person who owns the link.
Payload : { recipient : Str, small1 : Str, small2 : Str, big : Str, book : Str }

## Split each "Name BASE64" line on its first space
parse_gens : Str -> Try(List((Str, Str)), [BadLine(U64)])
parse_gens = |contents| {
	var $parsed = []
	var $line_number = 1
	for line in lines(contents) {
		match line.split_first(" ") {
			Ok({ before, after }) => {
				$parsed = $parsed.append((before, after))
			}
			Err(NotFound) => return Err(BadLine($line_number))
		}
		$line_number = $line_number + 1
	}
	Ok($parsed)
}

## Rebuild the draw from each person's link data, refusing data that does not
## re-encode to exactly the same text
rebuild : List((Str, Str)) -> Try(Draw.Assignments, [BadLine(U64), NotRoundTrip(U64), InvalidDraw])
rebuild = |people| {
	var $assignments = Dict.empty()
	var $line_number = 1
	for (name, data) in people {
		bytes = Base64.decode(data) ? |_| BadLine($line_number)
		text = Str.from_utf8(bytes) ? |_| BadLine($line_number)
		payload : Payload
		payload = Json.parse(text) ? |_| BadLine($line_number)
		if payload.recipient != name {
			return Err(BadLine($line_number))
		}
		reencoded = Base64.encode(Json.to_str(payload).to_utf8()) ? |_| NotRoundTrip($line_number)
		if reencoded != data {
			return Err(NotRoundTrip($line_number))
		}
		$assignments = $assignments.insert(name, { small1: payload.small1, small2: payload.small2, big: payload.big, book: payload.book })
		$line_number = $line_number + 1
	}
	if (Draw.is_valid(people.map(|(name, _)| name), $assignments)) Ok($assignments) else Err(InvalidDraw)
}

Message : { name : Str, message : Str }

## The follow-up text in kringle-bot's voice (see ../santa-game/COPY.md), with
## the link last
message_text : Str -> Str
message_text = |link|
	Str.join_with(
		[
			"ho-ho-hello! it's me, kringle-bot. i realized you might want to know who else is getting gifts for the people on your list, so you can strategize (⌐■_■)",
			"open it on the same device and browser you used last time and you won't have to redo my checks. or on a computer, cmd+shift+option+k (mac) or ctrl+shift+alt+k skips them anyway.",
			"i won't tell santa. (it will appear in access logs though) (^_^;)",
			link,
		],
		"\n\n",
	)

## Each person's follow-up text, with a link that adds other_givers
build_messages : Draw.Assignments -> Try(List(Message), [MissingGiver(Str), OutOfBounds, BadUtf8(_)])
build_messages = |assignments| {
	givers = Draw.gift_givers(assignments)?
	var $messages = []
	for (name, gifts) in assignments.to_list() {
		payload = {
			recipient: name,
			small1: gifts.small1,
			small2: gifts.small2,
			big: gifts.big,
			book: gifts.book,
			other_givers: Draw.other_givers(givers, gifts)?,
		}
		data = Base64.encode(Json.to_str(payload).to_utf8())?
		$messages = $messages.append({ name, message: message_text("${base_url}${data}") })
	}
	Ok($messages)
}

prepare! = || {
	people = parse_gens(read!(gens_file)?)?
	assignments = rebuild(people)?
	messages = build_messages(assignments)?
	write!(messages_file, Str.join_with(messages.map(Json.to_str), "\n").concat("\n"))?
	path(messages_file).set_permissions!(OwnerOnly) ? |_| WriteFailed(messages_file)
	Stdout.line!("Checked ${List.len(people).to_str()} links from ${gens_file}: the draw is valid and every link re-encodes exactly.")?
	Stdout.line!("Wrote ${List.len(messages).to_str()} messages to ${messages_file}.")
}

## ---- contacts ----

## Prints one line per phone of each matching person: name, label, number,
## separated by tabs. A person without phones gets one line with no number.
contacts_script : Str
contacts_script =
	\\on run argv
	\\  set target to item 1 of argv
	\\  set found to {}
	\\  tell application "Contacts"
	\\    repeat with p in (every person whose name is target or nickname is target or first name is target)
	\\      set pname to name of p
	\\      set ps to phones of p
	\\      if (count of ps) is 0 then
	\\        set end of found to pname & tab & tab
	\\      end if
	\\      repeat with ph in ps
	\\        set lbl to label of ph
	\\        if lbl is missing value then set lbl to ""
	\\        set end of found to pname & tab & lbl & tab & (value of ph)
	\\      end repeat
	\\    end repeat
	\\  end tell
	\\  set AppleScript's text item delimiters to linefeed
	\\  return found as text
	\\end run

Candidate : { contact : Str, label : Str, number : Str }

parse_candidates : Str -> List(Candidate)
parse_candidates = |output|
	lines(output).map(
		|line|
			match line.split_on("\t") {
				[contact, label, number] => { contact, label: clean_label(label), number }
				_ => { contact: line, label: "", number: "" }
			},
	)

## Contacts stores built-in labels like "_$!<Mobile>!$_"
clean_label : Str -> Str
clean_label = |label| label.drop_prefix("_$!<").drop_suffix(">!$_")

## A US phone number as +1 and ten digits, or a number already starting with +
normalize_phone : Str -> Try(Str, [UnrecognizedNumber])
normalize_phone = |number| {
	digits = number.to_utf8().keep_if(|c| c >= '0' and c <= '9')
	digit_str = Str.from_utf8(digits) ? |_| UnrecognizedNumber
	if number.trim().starts_with("+") and List.len(digits) >= 8 {
		Ok("+${digit_str}")
	} else if List.len(digits) == 10 {
		Ok("+1${digit_str}")
	} else if List.len(digits) == 11 and digits.first() == Ok('1') {
		Ok("+${digit_str}")
	} else {
		Err(UnrecognizedNumber)
	}
}

## What a Contacts search found: one person with one usable number, or a
## problem for the owner to settle
Lookup : [Found(Candidate, Str), NoMatch, Unclear(List(Candidate))]

decide : List(Candidate) -> Lookup
decide = |candidates|
	match candidates {
		[] => NoMatch
		[only] =>
			match normalize_phone(only.number) {
				Ok(phone) => Found(only, phone)
				Err(UnrecognizedNumber) => Unclear(candidates)
			}
		_ => Unclear(candidates)
	}

describe : Candidate -> Str
describe = |c| {
	label = if (c.label.is_empty()) "" else " (${c.label})"
	number = if (c.number.is_empty()) "no phone number" else c.number
	"${c.contact}${label}: ${number}"
}

## phones.json, one person per line so it is easy to edit by hand
phones_json : List((Str, Str)) -> Str
phones_json = |phones|
	match phones {
		[] => "{}\n"
		_ => {
			entries = phones.map(|(name, phone)| "  ${Json.to_str(name)}: ${Json.to_str(phone)}")
			"{\n${Str.join_with(entries, ",\n")}\n}\n"
		}
	}

contacts! = |aliases| {
	names = parse_gens(read!(gens_file)?)?.map(|(name, _)| name)
	saved_text = read_optional!(phones_file)?
	saved : Dict(Str, Str)
	saved = if (saved_text.is_empty()) Dict.empty() else Json.parse(saved_text) ? |_| ParseFailed(phones_file)

	var $phones = []
	var $problems = []
	for name in names {
		match saved.get(name) {
			Ok(phone) => {
				$phones = $phones.append((name, phone))
				Stdout.line!("${name}: ${phone} (already in ${phones_file})")?
			}
			Err(_) => {
				query = aliases.get(name) ?? name
				output = Cmd.capture!(OsStr.from_str("osascript"), ["-e", contacts_script, query].map(OsStr.from_str)) ? |_| ContactsFailed(name)
				match decide(parse_candidates(output)) {
					Found(candidate, phone) => {
						$phones = $phones.append((name, phone))
						Stdout.line!("${name}: ${phone} from contact ${describe(candidate)}")?
					}
					NoMatch => {
						$problems = $problems.append("${name}: no contact named \"${query}\"")
					}
					Unclear(candidates) => {
						found = Str.join_with(candidates.map(|c| "    ${describe(c)}"), "\n")
						$problems = $problems.append("${name}: no single phone number for \"${query}\"; found\n${found}")
					}
				}
			}
		}
	}

	write!(phones_file, phones_json($phones))?
	Stdout.line!("Saved ${List.len($phones).to_str()} of ${List.len(names).to_str()} phone numbers to ${phones_file}.")?
	if $problems.is_empty() {
		Ok({})
	} else {
		Err(Unresolved($problems))
	}
}

## ---- send ----

## Text the second argument to the handle in the first. Errors report only
## their number, so the message never shows up in the output.
send_script : Str
send_script =
	\\on run argv
	\\  try
	\\    tell application "Messages"
	\\      set svc to 1st account whose service type = iMessage
	\\      send (item 2 of argv) to participant (item 1 of argv) of svc
	\\    end tell
	\\  on error number n
	\\    error "Messages could not send the text (error " & n & ")" number n
	\\  end try
	\\  return ""
	\\end run

parse_messages : Str -> Try(List(Message), [BadMessageLine(U64)])
parse_messages = |contents| {
	var $messages = []
	var $line_number = 1
	for line in lines(contents) {
		message : Message
		message = Json.parse(line) ? |_| BadMessageLine($line_number)
		$messages = $messages.append(message)
		$line_number = $line_number + 1
	}
	Ok($messages)
}

## Pair each message with a phone number, or name everyone who has none
pair_phones : List(Message), Dict(Str, Str) -> Try(List({ name : Str, handle : Str, message : Str }), [MissingPhones(List(Str))])
pair_phones = |messages, phones| {
	var $paired = []
	var $missing = []
	for m in messages {
		match phones.get(m.name) {
			Ok(handle) => {
				$paired = $paired.append({ name: m.name, handle, message: m.message })
			}
			Err(_) => {
				$missing = $missing.append(m.name)
			}
		}
	}
	if ($missing.is_empty()) Ok($paired) else Err(MissingPhones($missing))
}

send! = |options| {
	messages = parse_messages(read!(messages_file)?)?
	phones : Dict(Str, Str)
	phones = Json.parse(read!(phones_file)?) ? |_| ParseFailed(phones_file)
	paired = pair_phones(messages, phones)?
	selected = match options.only {
		Only(name) => {
			chosen = paired.keep_if(|p| p.name == name)
			if chosen.is_empty() {
				return Err(UnknownName(name))
			}
			chosen
		}
		Everyone => paired
	}
	already_sent = Set.from_list(lines(read_optional!(sent_file)?))
	to_send = selected.keep_if(|p| !already_sent.contains(p.name))

	if !options.send {
		for p in selected {
			status = if (already_sent.contains(p.name)) "already sent, will skip" else "would send"
			Stdout.line!("${p.name} -> ${p.handle} (${status})")?
		}
		return Stdout.line!("Dry run: ${List.len(to_send).to_str()} to send. Pass --send to text them.")
	}

	var $first = True
	for p in to_send {
		if !$first {
			Sleep.millis!(send_pause)?
		}
		$first = False
		Cmd.call!(OsStr.from_str("osascript"), ["-e", send_script, p.handle, p.message].map(OsStr.from_str)) ? |_| SendFailed(p.name)
		path(sent_file).append_utf8!("${p.name}\n") ? |_| WriteFailed(sent_file)
		Stdout.line!("Sent to ${p.name} (${p.handle})")?
	}
	skipped = List.len(selected) - List.len(to_send)
	Stdout.line!("Sent ${List.len(to_send).to_str()} texts; skipped ${skipped.to_str()} already in ${sent_file}.")
}

run! = |os_args|
	match parse_args(os_args.map(OsStr.display))? {
		Help => Stdout.line!(usage)
		Prepare => prepare!()
		Contacts(aliases) => contacts!(aliases)
		Send(options) => send!(options)
	}

## Error messages name files, lines and people, never link contents
error_message = |err|
	match err {
		BadArgs(msg) => "${msg}\n\n${usage}"
		ReadFailed(name) => "Could not read ${name}"
		WriteFailed(name) => "Could not write ${name}"
		ParseFailed(name) => "Could not parse ${name}"
		BadLine(line) => "Line ${line.to_str()} of ${gens_file} is not a name and valid link data for that person"
		NotRoundTrip(line) => "Line ${line.to_str()} of ${gens_file} does not re-encode to the same link data"
		InvalidDraw => "The draw in ${gens_file} breaks the gift rules"
		MissingGiver(name) => "Bug: the draw has no giver for ${name}"
		OutOfBounds | BadUtf8(_) => "Bug: could not encode a link"
		ContactsFailed(name) => "Could not search Contacts for ${name}; check that this terminal may control Contacts in System Settings > Privacy & Security > Automation"
		Unresolved(problems) => "Could not find a phone number for everyone. Rerun with --as NAME CONTACT, or add them to ${phones_file} by hand:\n${Str.join_with(problems, "\n")}"
		BadMessageLine(line) => "Line ${line.to_str()} of ${messages_file} is not a name and message; rerun prepare"
		MissingPhones(names) => "No phone number in ${phones_file} for: ${Str.join_with(names, ", ")}. Nothing was sent."
		UnknownName(name) => "${name} is not in ${messages_file}"
		SendFailed(name) => "Could not text ${name}; stopped before anyone after them. Rerun to continue: ${sent_file} skips everyone already sent."
		Interrupted => "Interrupted"
		_ => "Unexpected error"
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

## ---- tests, with made-up names ----

test_people : List(Str)
test_people = ["Ada", "Bea", "Cy", "Dot", "Eli", "Fay"]

## Links in the past-gens format for a draw of the test people
test_gens : U32 -> Try(Str, _)
test_gens = |seed| {
	assignments = Draw.draw(test_people, Dict.empty(), seed)?
	var $lines = []
	for (name, gifts) in assignments.to_list() {
		payload : Payload
		payload = { recipient: name, small1: gifts.small1, small2: gifts.small2, big: gifts.big, book: gifts.book }
		data = Base64.encode(Json.to_str(payload).to_utf8())?
		$lines = $lines.append("${name} ${data}")
	}
	Ok(Str.join_with($lines, "\n").concat("\n"))
}

# A draw survives the trip through past-gens lines
expect {
	round_trip = |seed| {
		people = parse_gens(test_gens(seed)?)?
		assignments = rebuild(people)?
		Ok(people.map(|(name, _)| name) == test_people and Draw.is_valid(test_people, assignments) and assignments == Draw.draw(test_people, Dict.empty(), seed)?)
	}
	round_trip(3) == Ok(True)
}

# Data that decodes but re-encodes differently is refused
expect {
	padded = Base64.encode(" {\"big\":\"Cy\",\"book\":\"Dot\",\"recipient\":\"Ada\",\"small1\":\"Bea\",\"small2\":\"Eli\"}".to_utf8())
	match padded {
		Ok(data) => rebuild([("Ada", data)]) == Err(NotRoundTrip(1))
		Err(_) => False
	}
}

# A line owned by someone else, bad base64, and a missing space are refused
expect {
	refused = |seed| {
		people = parse_gens(test_gens(seed)?)?
		swapped = people.map(|(name, data)| if (name == "Ada") ("Bea", data) else if (name == "Bea") ("Ada", data) else (name, data))
		Ok(rebuild(swapped) == Err(BadLine(1)) and rebuild([("Ada", "Zg=")]) == Err(BadLine(1)) and parse_gens("Ada\n") == Err(BadLine(1)))
	}
	refused(3) == Ok(True)
}

# A draw that breaks the gift rules is refused
expect {
	bad = |seed| {
		people = parse_gens(test_gens(seed)?)?
		Ok(rebuild(people.drop_last(1)) == Err(InvalidDraw))
	}
	bad(3) == Ok(True)
}

# Each message has one link, whose payload adds other_givers to the original
expect {
	links = |seed| {
		assignments = Draw.draw(test_people, Dict.empty(), seed)?
		messages = build_messages(assignments)?
		givers = Draw.gift_givers(assignments)?
		var $all_ok = List.len(messages) == List.len(test_people)
		for m in messages {
			link_lines = m.message.split_on("\n").keep_if(|line| line.contains(base_url))
			gifts = assignments.get(m.name)?
			expected = Base64.encode(
				Json.to_str({
					recipient: m.name,
					small1: gifts.small1,
					small2: gifts.small2,
					big: gifts.big,
					book: gifts.book,
					other_givers: Draw.other_givers(givers, gifts)?,
				}).to_utf8(),
			)?
			$all_ok = $all_ok
				and link_lines == ["${base_url}${expected}"]
				and m.message.ends_with("\n${base_url}${expected}")
				and m.message == message_text("${base_url}${expected}")
		}
		Ok($all_ok)
	}
	links(5) == Ok(True)
}

# Phone numbers
expect normalize_phone("(555) 123-4567") == Ok("+15551234567")
expect normalize_phone("1 555 123 4567") == Ok("+15551234567")
expect normalize_phone("+44 20 7946 0958") == Ok("+442079460958")
expect normalize_phone("123-4567") == Err(UnrecognizedNumber)
expect normalize_phone("") == Err(UnrecognizedNumber)

# Contacts results
expect clean_label("_$!<Mobile>!$_") == "Mobile"
expect clean_label("iPhone") == "iPhone"
expect decide(parse_candidates("Ada Lovelace\t_$!<Mobile>!$_\t(555) 123-4567\n")) == Found({ contact: "Ada Lovelace", label: "Mobile", number: "(555) 123-4567" }, "+15551234567")
expect decide(parse_candidates("")) == NoMatch
expect decide(parse_candidates("Ada Lovelace\t\t\n")) == Unclear([{ contact: "Ada Lovelace", label: "", number: "" }])
expect
	decide(parse_candidates("Ada Lovelace\tMobile\t5551234567\nAda Lovelace\tHome\t5559876543\n"))
		== Unclear([{ contact: "Ada Lovelace", label: "Mobile", number: "5551234567" }, { contact: "Ada Lovelace", label: "Home", number: "5559876543" }])

# phones.json round trips
expect {
	phones = [("Ada", "+15551234567"), ("Bea \"B\"", "+15559876543")]
	parsed : Try(Dict(Str, Str), _)
	parsed = Json.parse(phones_json(phones))
	parsed == Ok(Dict.from_list(phones))
}

# Pairing stops if anyone lacks a phone number
expect {
	messages = [{ name: "Ada", message: "hi" }, { name: "Bea", message: "yo" }]
	pair_phones(messages, Dict.from_list([("Ada", "+15551234567")])) == Err(MissingPhones(["Bea"]))
		and pair_phones(messages, Dict.from_list([("Bea", "+2"), ("Ada", "+1")])) == Ok([{ name: "Ada", handle: "+1", message: "hi" }, { name: "Bea", handle: "+2", message: "yo" }])
}

# Arguments
expect parse_args(["send"]) == Ok(Send({ send: False, only: Everyone }))
expect parse_args(["send", "--only", "Ada", "--send"]) == Ok(Send({ send: True, only: Only("Ada") }))
expect parse_args(["contacts", "--as", "Mom", "Ada Lovelace"]) == Ok(Contacts(Dict.from_list([("Mom", "Ada Lovelace")])))
expect parse_args(["send", "--only"]) == Err(BadArgs("Unknown or incomplete argument to send: --only"))
