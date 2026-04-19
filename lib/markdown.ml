(** See markdown.mli for the public contract. *)

type state = {
	fenced_code : string option;
}

let init = { fenced_code = None }

type span = {
	start : int;
	stop  : int;
	style : Theme.style;
}

(* ------------------------------------------------------------------ *)
(* Low-level helpers                                                  *)
(* ------------------------------------------------------------------ *)

let is_space c = c = ' ' || c = '\t'

let rec leading_space s i =
	if i >= String.length s then i
	else if is_space s.[i] then leading_space s (i + 1)
	else i

let match_at s i ch =
	i < String.length s && s.[i] = ch

let string_at s i substr =
	let sl = String.length substr in
	i + sl <= String.length s
	&& String.sub s i sl = substr

(** Find the next occurrence of [substr] starting at [i]. Returns
    the index, or [None] if not found. *)
let find_from s substr i =
	let sl = String.length s in
	let nl = String.length substr in
	if nl = 0 then Some i
	else
		let limit = sl - nl in
		let rec loop j =
			if j > limit then None
			else if string_at s j substr then Some j
			else loop (j + 1)
		in
		loop i

(* ------------------------------------------------------------------ *)
(* Fenced code detection                                              *)
(* ------------------------------------------------------------------ *)

(** Returns [Some lang] if [line]'s trimmed prefix is "```" (with
    an optional language tag following). Empty lang string means
    no tag. *)
let fence_line line =
	let i = leading_space line 0 in
	if i + 3 <= String.length line
	   && line.[i] = '`' && line.[i + 1] = '`' && line.[i + 2] = '`'
	then
		let rest_start = i + 3 in
		let lang =
			String.trim (String.sub line rest_start
				(String.length line - rest_start))
		in
		Some lang
	else None

(* ------------------------------------------------------------------ *)
(* Block classification                                               *)
(* ------------------------------------------------------------------ *)

type range = { r_start : int; r_stop : int }

type block_kind =
	| Heading of int
	| List_marker of range
		(** Range of the marker (including trailing space). *)
	| Blockquote
	| Hr
	| Table_row
	| Normal

let heading_level line =
	let i = leading_space line 0 in
	let rec count j =
		if j < String.length line && line.[j] = '#' then count (j + 1)
		else j - i
	in
	let n = count i in
	if n >= 1 && n <= 6
	   && i + n < String.length line
	   && line.[i + n] = ' '
	then Some n
	else None

let is_hr_line line =
	let i = leading_space line 0 in
	let trimmed_end =
		let rec loop j =
			if j <= i then i
			else if is_space line.[j - 1] then loop (j - 1)
			else j
		in
		loop (String.length line)
	in
	let n = trimmed_end - i in
	if n < 3 then false
	else
		let c = line.[i] in
		if c = '-' || c = '*' || c = '_' then begin
			let rec all_same j =
				if j >= trimmed_end then true
				else if line.[j] = c then all_same (j + 1)
				else false
			in
			all_same i
		end else false

let list_marker_range line =
	let i = leading_space line 0 in
	let sl = String.length line in
	if i >= sl then None
	else
		let c = line.[i] in
		if (c = '-' || c = '*' || c = '+')
		   && i + 1 < sl
		   && line.[i + 1] = ' '
		then Some { r_start = i; r_stop = i + 2 }
		else if c >= '0' && c <= '9' then
			let rec digits j =
				if j < sl && line.[j] >= '0' && line.[j] <= '9'
				then digits (j + 1)
				else j
			in
			let d = digits i in
			if d > i && d + 1 < sl
			   && line.[d] = '.' && line.[d + 1] = ' '
			then Some { r_start = i; r_stop = d + 2 }
			else None
		else None

let is_blockquote line =
	let i = leading_space line 0 in
	i < String.length line && line.[i] = '>'

let is_table_row line =
	(* Cheap heuristic: line (after leading space) starts with '|'
	   and contains another '|' somewhere after. *)
	let i = leading_space line 0 in
	let sl = String.length line in
	if i >= sl || line.[i] <> '|' then false
	else
		let rec has_another j =
			if j >= sl then false
			else if line.[j] = '|' then true
			else has_another (j + 1)
		in
		has_another (i + 1)

let classify_block line =
	match heading_level line with
	| Some n -> Heading n
	| None ->
			if is_hr_line line then Hr
			else if is_blockquote line then Blockquote
			else if is_table_row line then Table_row
			else
				match list_marker_range line with
				| Some range -> List_marker range
				| None -> Normal

(* ------------------------------------------------------------------ *)
(* Inline tokenizer                                                   *)
(* ------------------------------------------------------------------ *)

(** Emit a span for [start..stop) with [style] into [acc].
    Drops empty ranges. *)
let emit acc ~start ~stop ~style =
	if stop > start then { start; stop; style } :: acc
	else acc

(** Scan [line] from [from] to [stop] emitting inline-styled
    spans. Styles applied: inline_code, bold, italic,
    strikethrough, link. Everything else gets [plain].

    The scanner is greedy and non-overlapping: when it opens
    bold at position i and finds a close at j, bytes [i..j+2)
    are one span; scanning resumes at [j+2]. Unmatched opens
    degrade to plain (the whole line renders unstyled if a
    [**] has no closer). *)
let tokenize_inline line ~from ~stop ~(theme : Theme.markdown) =
	let acc = ref [] in
	let pos = ref from in
	let plain_start = ref from in
	let flush_plain_up_to p =
		if p > !plain_start then
			acc := emit !acc ~start:!plain_start ~stop:p ~style:Theme.plain
	in
	let take_styled ~open_len ~close ~style =
		let after_open = !pos + open_len in
		match find_from line close after_open with
		| None -> false
		| Some close_i when close_i < stop ->
				flush_plain_up_to !pos;
				let span_stop = close_i + String.length close in
				acc := emit !acc ~start:!pos ~stop:span_stop ~style;
				pos := span_stop;
				plain_start := span_stop;
				true
		| Some _ -> false
	in
	let take_link () =
		(* [label](url) — consume to the closing paren *)
		let sl = stop in
		if !pos >= sl || line.[!pos] <> '[' then false
		else
			match find_from line "]" (!pos + 1) with
			| Some end_label when end_label + 1 < sl
			                       && line.[end_label + 1] = '(' ->
					(match find_from line ")" (end_label + 2) with
					 | Some end_paren when end_paren < sl ->
							 flush_plain_up_to !pos;
							 let span_stop = end_paren + 1 in
							 acc := emit !acc ~start:!pos ~stop:span_stop
								 ~style:theme.link;
							 pos := span_stop;
							 plain_start := span_stop;
							 true
					 | _ -> false)
			| _ -> false
	in
	while !pos < stop do
		let consumed =
			if string_at line !pos "**" then
				take_styled ~open_len:2 ~close:"**" ~style:theme.bold
			else if string_at line !pos "~~" then
				take_styled ~open_len:2 ~close:"~~"
					~style:theme.strikethrough
			else if match_at line !pos '`' then
				take_styled ~open_len:1 ~close:"`"
					~style:theme.inline_code
			else if match_at line !pos '*' then
				take_styled ~open_len:1 ~close:"*" ~style:theme.italic
			else if match_at line !pos '_' then
				take_styled ~open_len:1 ~close:"_" ~style:theme.italic
			else if match_at line !pos '[' then
				take_link ()
			else false
		in
		if not consumed then incr pos
	done;
	flush_plain_up_to stop;
	List.rev !acc

(* ------------------------------------------------------------------ *)
(* Table row styling (no cross-line state)                            *)
(* ------------------------------------------------------------------ *)

(** Style each `|` character with [table_border]; everything
    between and after is plain. This gives LLM-produced tables
    visible column separators without needing to confirm the
    line is part of an actual table structure. *)
let tokenize_table_row line ~(theme : Theme.markdown) =
	let sl = String.length line in
	let acc = ref [] in
	let plain_start = ref 0 in
	for i = 0 to sl - 1 do
		if line.[i] = '|' then begin
			if i > !plain_start then
				acc := emit !acc ~start:!plain_start ~stop:i
					~style:Theme.plain;
			acc := emit !acc ~start:i ~stop:(i + 1)
				~style:theme.table_border;
			plain_start := i + 1
		end
	done;
	if !plain_start < sl then
		acc := emit !acc ~start:!plain_start ~stop:sl
			~style:Theme.plain;
	List.rev !acc

(* ------------------------------------------------------------------ *)
(* Main                                                               *)
(* ------------------------------------------------------------------ *)

let whole_line_span line ~style =
	[ { start = 0; stop = String.length line; style } ]

let heading_style (theme : Theme.markdown) n =
	(* theme.heading is a 6-element array indexed 0..5 *)
	let i = max 0 (min 5 (n - 1)) in
	theme.heading.(i)

let tokenize_line ~state ~line ~theme =
	let sl = String.length line in
	match state.fenced_code with
	| Some _ ->
			(* Inside a fenced block. The line is styled as code.
			   If this line itself is a ``` fence, close it. *)
			let style = theme.Theme.code_block in
			let spans =
				if sl = 0 then
					[ { start = 0; stop = 0; style = Theme.plain } ]
				else
					whole_line_span line ~style
			in
			let new_state =
				match fence_line line with
				| Some _ -> { fenced_code = None }
				| None -> state
			in
			(spans, new_state)
	| None ->
			(* Not in a fenced block. Classify and tokenize. *)
			match fence_line line with
			| Some lang ->
					let style = theme.Theme.code_block in
					let spans =
						if sl = 0 then
							[ { start = 0; stop = 0; style = Theme.plain } ]
						else
							whole_line_span line ~style
					in
					(spans, { fenced_code = Some lang })
			| None ->
					let block = classify_block line in
					let spans =
						match block with
						| Hr ->
								whole_line_span line ~style:theme.Theme.hr
						| Heading n ->
								whole_line_span line
									~style:(heading_style theme n)
						| Blockquote ->
								whole_line_span line
									~style:theme.Theme.blockquote
						| Table_row ->
								tokenize_table_row line ~theme
						| List_marker range ->
								let acc = ref [] in
								if range.r_start > 0 then
									acc := emit !acc ~start:0
										~stop:range.r_start
										~style:Theme.plain;
								acc := emit !acc ~start:range.r_start
									~stop:range.r_stop
									~style:theme.Theme.list_marker;
								let inline = tokenize_inline line
									~from:range.r_stop ~stop:sl ~theme
								in
								List.rev_append !acc inline
						| Normal ->
								tokenize_inline line ~from:0 ~stop:sl ~theme
					in
					let spans =
						if spans = [] then
							[ { start = 0; stop = 0; style = Theme.plain } ]
						else spans
					in
					(spans, state)
