type direction = Up | Down | Left | Right

type event =
	| Char of Uchar.t
	| Enter
	| Backspace
	| Tab
	| Escape
	| Arrow of direction
	| Home
	| End
	| Page_up
	| Page_down
	| Delete
	| Word_left
	| Word_right
	| Doc_home
	| Doc_end
	| Ctrl of char
	| Eof
	| Unknown

exception Eof_reached

let read_byte () =
	let buf = Bytes.create 1 in
	try
		let n = Unix.read Unix.stdin buf 0 1 in
		if n = 0 then raise Eof_reached
		else Some (Bytes.get_uint8 buf 0)
	with Unix.Unix_error (Unix.EINTR, _, _) -> None

let input_ready ?(timeout = 0.05) () =
	try
		let ready, _, _ = Unix.select [Unix.stdin] [] [] timeout in
		ready <> []
	with _ -> false

let read_escape_sequence () =
	if not (input_ready ()) then Escape
	else
		match read_byte () with
		| None -> Escape
		| Some 0x5B ->
			(match read_byte () with
			| Some 0x41 -> Arrow Up
			| Some 0x42 -> Arrow Down
			| Some 0x43 -> Arrow Right
			| Some 0x44 -> Arrow Left
			| Some 0x48 -> Home
			| Some 0x46 -> End
			| Some b when b >= 0x30 && b <= 0x39 ->
				let buf = Buffer.create 8 in
				Buffer.add_char buf (Char.chr b);
				let rec loop () =
					match read_byte () with
					| None -> ()
					| Some c ->
						Buffer.add_char buf (Char.chr c);
						if c = 0x7E || (c >= 0x40 && c <= 0x7E) then ()
						else loop ()
				in
				loop ();
				(match Buffer.contents buf with
				| "1~" | "7~" -> Home
				| "4~" | "8~" -> End
				| "3~" -> Delete
				| "5~" -> Page_up
				| "6~" -> Page_down
				| "1;5C" -> Word_right
				| "1;5D" -> Word_left
				| "1;5H" -> Doc_home
				| "1;5F" -> Doc_end
				| _ -> Unknown)
			| _ -> Unknown)
		| Some 0x4F ->
			(match read_byte () with
			| Some 0x48 -> Home
			| Some 0x46 -> End
			| _ -> Unknown)
		| _ -> Unknown

let decode_utf8 lead =
	let nbytes =
		if lead < 0x80 then 1
		else if lead < 0xC0 then 1
		else if lead < 0xE0 then 2
		else if lead < 0xF0 then 3
		else 4
	in
	let buf = Bytes.create nbytes in
	Bytes.set_uint8 buf 0 lead;
	let ok = ref true in
	for i = 1 to nbytes - 1 do
		match read_byte () with
		| Some b -> Bytes.set_uint8 buf i b
		| None -> ok := false
	done;
	if not !ok then None
	else begin
		let result = ref None in
		Uutf.String.fold_utf_8
			(fun () _ dec ->
				match dec with
				| `Uchar u -> if !result = None then result := Some u
				| `Malformed _ -> ())
			() (Bytes.to_string buf);
		!result
	end

let rec read () =
	try
		match read_byte () with
		| None -> read ()
		| Some 0x1B -> read_escape_sequence ()
		| Some 0x0D | Some 0x0A -> Enter
		| Some 0x09 -> Tab
		| Some 0x7F -> Backspace
		| Some b when b < 0x20 -> Ctrl (Char.chr (b + 0x60))
		| Some b when b < 0x80 -> Char (Uchar.of_int b)
		| Some b ->
			(match decode_utf8 b with
			| Some u -> Char u
			| None -> Unknown)
	with Eof_reached -> Eof
