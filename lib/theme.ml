(** Theme implementation. See theme.mli for the public contract. *)

(* ------------------------------------------------------------------ *)
(* Types                                                              *)
(* ------------------------------------------------------------------ *)

type color =
	| Default
	| Ansi of int
	| Ansi256 of int
	| Rgb of int * int * int

type style = {
	fg : color;
	bg : color;
	bold : bool;
	dim : bool;
	italic : bool;
	underline : bool;
	strikethrough : bool;
}

type chrome = {
	status            : style;
	title_focused     : style;
	title_unfocused   : style;
	border_focused    : style;
	tab_active        : style;
	tab_inactive      : style;
	warning           : style;
	error             : style;
}

type markdown = {
	heading          : style array;
	code_block       : style;
	inline_code      : style;
	link             : style;
	bold             : style;
	italic           : style;
	strikethrough    : style;
	blockquote       : style;
	list_marker      : style;
	hr               : style;
	table_border     : style;
	table_header     : style;
}

type syntax = {
	text             : style;
	keyword          : style;
	type_kw          : style;
	string_lit       : style;
	char_lit         : style;
	number           : style;
	comment          : style;
	preproc          : style;
	selection        : style;
	match_highlight  : style;
	bracket_colors   : style array;
	bracket_match    : style;
	gutter_added     : style;
	gutter_modified  : style;
}

type t = {
	chrome   : chrome;
	markdown : markdown;
	syntax   : syntax;
}

(* ------------------------------------------------------------------ *)
(* Constants                                                          *)
(* ------------------------------------------------------------------ *)

let reset = "\x1b[0m"

let plain = {
	fg = Default;
	bg = Default;
	bold = false;
	dim = false;
	italic = false;
	underline = false;
	strikethrough = false;
}

(* ------------------------------------------------------------------ *)
(* ANSI emission                                                      *)
(* ------------------------------------------------------------------ *)

let fg_params = function
	| Default -> [ "39" ]
	| Ansi n when n >= 0 && n <= 7 -> [ string_of_int (30 + n) ]
	| Ansi n when n >= 8 && n <= 15 -> [ string_of_int (90 + (n - 8)) ]
	| Ansi _ -> [ "39" ]   (* out of range, fall back to default *)
	| Ansi256 n -> [ "38"; "5"; string_of_int n ]
	| Rgb (r, g, b) ->
			[ "38"; "2"; string_of_int r;
			  string_of_int g; string_of_int b ]

let bg_params = function
	| Default -> [ "49" ]
	| Ansi n when n >= 0 && n <= 7 -> [ string_of_int (40 + n) ]
	| Ansi n when n >= 8 && n <= 15 -> [ string_of_int (100 + (n - 8)) ]
	| Ansi _ -> [ "49" ]
	| Ansi256 n -> [ "48"; "5"; string_of_int n ]
	| Rgb (r, g, b) ->
			[ "48"; "2"; string_of_int r;
			  string_of_int g; string_of_int b ]

let style_to_ansi s =
	if s = plain then ""
	else begin
		let parts = ref [] in
		if s.bold then parts := "1" :: !parts;
		if s.dim then parts := "2" :: !parts;
		if s.italic then parts := "3" :: !parts;
		if s.underline then parts := "4" :: !parts;
		if s.strikethrough then parts := "9" :: !parts;
		if s.fg <> Default then
			parts := List.rev_append (fg_params s.fg) !parts;
		if s.bg <> Default then
			parts := List.rev_append (bg_params s.bg) !parts;
		let all = List.rev !parts in
		if all = [] then ""
		else "\x1b[" ^ String.concat ";" all ^ "m"
	end

(* ------------------------------------------------------------------ *)
(* Color parsing                                                      *)
(* ------------------------------------------------------------------ *)

let named_color = function
	| "black" -> Some (Ansi 0)
	| "red" -> Some (Ansi 1)
	| "green" -> Some (Ansi 2)
	| "yellow" -> Some (Ansi 3)
	| "blue" -> Some (Ansi 4)
	| "magenta" -> Some (Ansi 5)
	| "cyan" -> Some (Ansi 6)
	| "white" -> Some (Ansi 7)
	| "bright_black" -> Some (Ansi 8)
	| "bright_red" -> Some (Ansi 9)
	| "bright_green" -> Some (Ansi 10)
	| "bright_yellow" -> Some (Ansi 11)
	| "bright_blue" -> Some (Ansi 12)
	| "bright_magenta" -> Some (Ansi 13)
	| "bright_cyan" -> Some (Ansi 14)
	| "bright_white" -> Some (Ansi 15)
	| _ -> None

let hex_digit c =
	if c >= '0' && c <= '9' then Some (Char.code c - Char.code '0')
	else if c >= 'a' && c <= 'f' then Some (10 + Char.code c - Char.code 'a')
	else if c >= 'A' && c <= 'F' then Some (10 + Char.code c - Char.code 'A')
	else None

let parse_hex6 s =
	if String.length s <> 6 then None
	else
		let digit i =
			match hex_digit s.[i] with
			| Some d -> d
			| None -> raise Exit
		in
		try
			let r = digit 0 * 16 + digit 1 in
			let g = digit 2 * 16 + digit 3 in
			let b = digit 4 * 16 + digit 5 in
			Some (Rgb (r, g, b))
		with Exit -> None

let parse_color s =
	let s = String.lowercase_ascii (String.trim s) in
	if s = "" || s = "default" || s = "-" then Ok Default
	else if String.length s >= 1 && s.[0] = '#' then
		match parse_hex6 (String.sub s 1 (String.length s - 1)) with
		| Some c -> Ok c
		| None -> Error (Printf.sprintf "bad hex color: %S" s)
	else
		match named_color s with
		| Some c -> Ok c
		| None ->
				match int_of_string_opt s with
				| Some n when n >= 0 && n <= 15 -> Ok (Ansi n)
				| Some n when n >= 16 && n <= 255 -> Ok (Ansi256 n)
				| Some n ->
						Error (Printf.sprintf "color index out of range: %d" n)
				| None -> Error (Printf.sprintf "unknown color: %S" s)

(* ------------------------------------------------------------------ *)
(* Default theme                                                      *)
(* ------------------------------------------------------------------ *)

let s ?(fg = Default) ?(bg = Default) ?(bold = false)
    ?(dim = false) ?(italic = false) ?(underline = false)
    ?(strikethrough = false) () =
	{ fg; bg; bold; dim; italic; underline; strikethrough }

let default_chrome = {
	status           = plain;
	title_focused    = s ~fg:(Ansi256 231) ~bg:(Ansi256 24) ~bold:true ();
	title_unfocused  = s ~fg:(Ansi256 245) ();
	border_focused   = s ~fg:(Ansi256 24) ();
	tab_active       = s ~fg:(Ansi256 231) ~bg:(Ansi256 24) ~bold:true ();
	tab_inactive     = s ~fg:(Ansi256 245) ();
	warning          = s ~fg:(Ansi 3) ();
	error            = s ~fg:(Ansi 1) ~bold:true ();
}

let default_markdown = {
	heading = [|
		s ~fg:(Ansi256 33)  ~bold:true ();
		s ~fg:(Ansi256 45)  ~bold:true ();
		s ~fg:(Ansi256 75)  ~bold:true ();
		s ~fg:(Ansi256 75) ();
		s ~fg:(Ansi256 111) ();
		s ~fg:(Ansi256 147) ();
	|];
	code_block    = s ~fg:(Ansi256 244) ();
	inline_code   = s ~fg:(Ansi256 244) ();
	link          = s ~fg:(Ansi256 75) ~underline:true ();
	bold          = s ~bold:true ();
	italic        = s ~italic:true ();
	strikethrough = s ~strikethrough:true ();
	blockquote    = s ~fg:(Ansi256 245) ~dim:true ();
	list_marker   = s ~fg:(Ansi256 75) ();
	hr            = s ~fg:(Ansi256 240) ~dim:true ();
	table_border  = s ~fg:(Ansi256 240) ~dim:true ();
	table_header  = s ~bold:true ();
}

(* twig-edit's current default theme values, translated into the
   structured form. Preserving exact visual parity after 0c-ii
   migrates twig-edit to consume this. *)
let default_syntax = {
	text             = plain;
	keyword          = s ~fg:(Ansi 5) ~bold:true ();   (* \x1b[1;35m *)
	type_kw          = s ~fg:(Ansi 6) ();              (* \x1b[36m   *)
	string_lit       = s ~fg:(Ansi 2) ();              (* \x1b[32m   *)
	char_lit         = s ~fg:(Ansi 2) ();
	number           = s ~fg:(Ansi 3) ();              (* \x1b[33m   *)
	comment          = s ~fg:(Ansi 8) ();              (* \x1b[90m   *)
	preproc          = s ~fg:(Ansi 1) ();              (* \x1b[31m   *)
	selection        = s ~fg:(Ansi256 231) ~bg:(Ansi256 24) ();
	match_highlight  = s ~fg:(Ansi256 16) ~bg:(Ansi256 226) ();
	bracket_colors   = [|
		s ~fg:(Ansi256 178) ();
		s ~fg:(Ansi256 141) ();
		s ~fg:(Ansi256 73) ();
		s ~fg:(Ansi256 209) ();
	|];
	bracket_match    = s ~bg:(Ansi256 238) ~bold:true ();
	gutter_added     = s ~fg:(Ansi256 114) ();
	gutter_modified  = s ~fg:(Ansi256 179) ();
}

let default = {
	chrome   = default_chrome;
	markdown = default_markdown;
	syntax   = default_syntax;
}

(* ------------------------------------------------------------------ *)
(* TOML loading                                                       *)
(* ------------------------------------------------------------------ *)

let err fmt = Printf.ksprintf (fun s -> Error (`Parse_error s)) fmt

(* Helpers over an Otoml.t root *)

let get_table_opt root path =
	try
		let v = Otoml.find root Otoml.get_table path in
		Some v
	with Not_found | Otoml.Key_error _ | Otoml.Type_error _ -> None

let get_string_opt root path =
	try Some (Otoml.find root Otoml.get_string path)
	with Not_found | Otoml.Key_error _ | Otoml.Type_error _ -> None

let get_bool_opt root path =
	try Some (Otoml.find root Otoml.get_boolean path)
	with Not_found | Otoml.Key_error _ | Otoml.Type_error _ -> None

let parse_style_from_table ~fallback root path =
	match get_table_opt root path with
	| None -> Ok fallback
	| Some _ ->
			let fg =
				match get_string_opt root (path @ [ "fg" ]) with
				| None -> Ok fallback.fg
				| Some s -> parse_color s
			in
			let bg =
				match get_string_opt root (path @ [ "bg" ]) with
				| None -> Ok fallback.bg
				| Some s -> parse_color s
			in
			match fg, bg with
			| Error msg, _ ->
					err "bad fg at %s: %s" (String.concat "." path) msg
			| _, Error msg ->
					err "bad bg at %s: %s" (String.concat "." path) msg
			| Ok fg, Ok bg ->
					let b k default =
						match get_bool_opt root (path @ [ k ]) with
						| Some v -> v
						| None -> default
					in
					Ok {
						fg; bg;
						bold = b "bold" fallback.bold;
						dim = b "dim" fallback.dim;
						italic = b "italic" fallback.italic;
						underline = b "underline" fallback.underline;
						strikethrough = b "strikethrough" fallback.strikethrough;
					}

let ( let* ) = Result.bind

(* Generic: parse a list of labelled styles under a common prefix,
   falling back per-field to [default_fields]. Returns a list of
   styles in the same order. *)
let parse_labelled root prefix ~fallbacks ~labels =
	let rec loop acc = function
		| [] -> Ok (List.rev acc)
		| (label, fallback) :: rest ->
				let path = prefix @ [ label ] in
				(match parse_style_from_table ~fallback root path with
				| Error e -> Error e
				| Ok v -> loop (v :: acc) rest)
	in
	loop [] (List.map2 (fun l f -> (l, f)) labels fallbacks)

let parse_chrome root =
	let fb = default_chrome in
	let labels =
		[ "status"; "title_focused"; "title_unfocused";
		  "border_focused";
		  "tab_active"; "tab_inactive";
		  "warning"; "error" ]
	in
	let fallbacks =
		[ fb.status; fb.title_focused; fb.title_unfocused;
		  fb.border_focused;
		  fb.tab_active; fb.tab_inactive;
		  fb.warning; fb.error ]
	in
	let* xs = parse_labelled root [ "chrome" ] ~labels ~fallbacks in
	match xs with
	| [ status; tf; tu; bf; ta; ti; w; e ] ->
			Ok {
				status = status; title_focused = tf; title_unfocused = tu;
				border_focused = bf;
				tab_active = ta; tab_inactive = ti;
				warning = w; error = e;
			}
	| _ -> err "internal: parse_chrome length mismatch"

let parse_markdown root =
	let fb = default_markdown in
	(* Headings via heading1..heading6 *)
	let heading_labels =
		[ "heading1"; "heading2"; "heading3";
		  "heading4"; "heading5"; "heading6" ]
	in
	let heading_fbs = Array.to_list fb.heading in
	let* hs = parse_labelled root [ "markdown" ]
		~labels:heading_labels ~fallbacks:heading_fbs in
	let heading = Array.of_list hs in
	let other_labels =
		[ "code_block"; "inline_code"; "link"; "bold"; "italic";
		  "strikethrough"; "blockquote"; "list_marker"; "hr";
		  "table_border"; "table_header" ]
	in
	let other_fbs =
		[ fb.code_block; fb.inline_code; fb.link; fb.bold; fb.italic;
		  fb.strikethrough; fb.blockquote; fb.list_marker; fb.hr;
		  fb.table_border; fb.table_header ]
	in
	let* os = parse_labelled root [ "markdown" ]
		~labels:other_labels ~fallbacks:other_fbs in
	match os with
	| [ cb; ic; lk; bo; it; st; bq; lm; hr; tb; th ] ->
			Ok {
				heading;
				code_block = cb; inline_code = ic; link = lk;
				bold = bo; italic = it; strikethrough = st;
				blockquote = bq; list_marker = lm; hr;
				table_border = tb; table_header = th;
			}
	| _ -> err "internal: parse_markdown length mismatch"

let parse_syntax root =
	let fb = default_syntax in
	let labels =
		[ "text"; "keyword"; "type_kw"; "string_lit"; "char_lit";
		  "number"; "comment"; "preproc";
		  "selection"; "match_highlight";
		  "bracket_match"; "gutter_added"; "gutter_modified" ]
	in
	let fallbacks =
		[ fb.text; fb.keyword; fb.type_kw; fb.string_lit; fb.char_lit;
		  fb.number; fb.comment; fb.preproc;
		  fb.selection; fb.match_highlight;
		  fb.bracket_match; fb.gutter_added; fb.gutter_modified ]
	in
	let* xs = parse_labelled root [ "syntax" ]
		~labels ~fallbacks in
	let bracket_labels =
		[ "bracket_color1"; "bracket_color2";
		  "bracket_color3"; "bracket_color4" ]
	in
	let bracket_fbs = Array.to_list fb.bracket_colors in
	let* bs = parse_labelled root [ "syntax" ]
		~labels:bracket_labels ~fallbacks:bracket_fbs in
	let bracket_colors = Array.of_list bs in
	match xs with
	| [ text; kw; tk; sl; cl; num; cm; pp; sel; mh; bm; ga; gm ] ->
			Ok {
				text; keyword = kw; type_kw = tk; string_lit = sl;
				char_lit = cl; number = num; comment = cm; preproc = pp;
				selection = sel; match_highlight = mh;
				bracket_colors; bracket_match = bm;
				gutter_added = ga; gutter_modified = gm;
			}
	| _ -> err "internal: parse_syntax length mismatch"

let parse_root root =
	let* chrome = parse_chrome root in
	let* markdown = parse_markdown root in
	let* syntax = parse_syntax root in
	Ok { chrome; markdown; syntax }

let load_string str =
	match Otoml.Parser.from_string_result str with
	| Error msg -> Error (`Parse_error msg)
	| Ok root -> parse_root root

let load ~path =
	match
		try Ok (Otoml.Parser.from_file path)
		with
		| Sys_error msg -> Error msg
		| Otoml.Parse_error (_, msg) -> Error msg
	with
	| Error msg -> Error (`Parse_error msg)
	| Ok root -> parse_root root
