type direction = Up | Down | Left | Right

type event =
	| Char of Uchar.t
	| Enter
	| Backspace
	| Tab
	| Shift_tab
	| Escape
	| Arrow of direction
	| Shift_arrow of direction
	| Home
	| End
	| Page_up
	| Page_down
	| Delete
	| Word_left
	| Word_right
	| Doc_home
	| Doc_end
	| Shift_space
	| Shift_enter
	| Alt of char
	| Ctrl of char
	| Eof
	| Resize
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
			| Some 0x5A -> Shift_tab
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
				let seq = Buffer.contents buf in
				let parse_csi_u s =
					let len = String.length s in
					if len < 2 || s.[len - 1] <> 'u' then None
					else begin
						let content = String.sub s 0 (len - 1) in
						let fields = String.split_on_char ';' content in
						match fields with
						| [] -> None
						| cp_s :: rest ->
							(match int_of_string_opt cp_s with
							| None -> None
							| Some cp ->
								let mod_val, text_cp =
									match rest with
									| [] -> (1, None)
									| [m] ->
										let mv =
											match int_of_string_opt m with
											| Some v -> v
											| None -> 1
										in
										(mv, None)
									| m :: t :: _ ->
										let mv =
											match int_of_string_opt m with
											| Some v -> v
											| None -> 1
										in
										let tv =
											match String.split_on_char ':' t with
											| [] -> None
											| first :: _ -> int_of_string_opt first
										in
										(mv, tv)
								in
								let mods = mod_val - 1 in
								let shift = mods land 1 <> 0 in
								let alt = mods land 2 <> 0 in
								let ctrl = mods land 4 <> 0 in
								Some (cp, shift, alt, ctrl, text_cp))
					end
				in
				let decode_csi_u cp shift alt ctrl text_cp =
					if cp = 27 then Escape
					else if cp = 13 && shift then Shift_enter
					else if cp = 13 then Enter
					else if cp = 9 && shift then Shift_tab
					else if cp = 9 then Tab
					else if cp = 127 then Backspace
					else if cp = 32 && shift then Shift_space
					else if ctrl && cp >= 97 && cp <= 122 then
						Ctrl (Char.chr cp)
					else if alt && cp >= 32 && cp < 127 then
						Alt (Char.chr cp)
					else if (not ctrl) && (not alt) then begin
						match text_cp with
						| Some t when t >= 32 ->
							Char (Uchar.of_int t)
						| _ ->
							if cp >= 32 && cp < 127 then begin
								let effective =
									if shift && cp >= 97 && cp <= 122 then
										cp - 32
									else cp
								in
								Char (Uchar.of_int effective)
							end
							else Unknown
					end
					else Unknown
				in
				(match seq with
				| "1~" | "7~" -> Home
				| "4~" | "8~" -> End
				| "3~" -> Delete
				| "5~" -> Page_up
				| "6~" -> Page_down
				| "1;5C" -> Word_right
				| "1;5D" -> Word_left
				| "1;5H" -> Doc_home
				| "1;5F" -> Doc_end
				| "1;2A" -> Shift_arrow Up
				| "1;2B" -> Shift_arrow Down
				| "1;2C" -> Shift_arrow Right
				| "1;2D" -> Shift_arrow Left
				| _ ->
					(match parse_csi_u seq with
					| Some (cp, shift, alt, ctrl, text_cp) ->
						decode_csi_u cp shift alt ctrl text_cp
					| None -> Unknown))
			| _ -> Unknown)
		| Some 0x4F ->
			(match read_byte () with
			| Some 0x48 -> Home
			| Some 0x46 -> End
			| _ -> Unknown)
		| Some b when b >= 0x20 && b < 0x7F -> Alt (Char.chr b)
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

let wake_flag : bool Atomic.t = Atomic.make false

let wake () = Atomic.set wake_flag true

let rec read () =
	if Atomic.compare_and_set wake_flag true false then Unknown
	else if Terminal.resize_flag () then Resize
	else begin
		let ready =
			try
				let r, _, _ = Unix.select [Unix.stdin] [] [] 0.1 in
				r
			with Unix.Unix_error (Unix.EINTR, _, _) -> []
		in
		if ready = [] then read ()
		else
			try
				match read_byte () with
				| None ->
					if Terminal.resize_flag () then Resize
					else read ()
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
	end
