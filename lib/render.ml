let esc = "\x1b["

let hide_cursor buf =
	Buffer.add_string buf esc;
	Buffer.add_string buf "?25l"

let show_cursor buf =
	Buffer.add_string buf esc;
	Buffer.add_string buf "?25h"

let move_cursor buf row col =
	Buffer.add_string buf esc;
	Buffer.add_string buf (string_of_int (row + 1));
	Buffer.add_char buf ';';
	Buffer.add_string buf (string_of_int (col + 1));
	Buffer.add_char buf 'H'

let clear_line buf =
	Buffer.add_string buf esc;
	Buffer.add_string buf "K"

let reverse_video buf =
	Buffer.add_string buf esc;
	Buffer.add_string buf "7m"

let reset_attrs buf =
	Buffer.add_string buf esc;
	Buffer.add_string buf "m"

let tab_width = 8

let iter_clusters f s =
	Uuseg_string.fold_utf_8 `Grapheme_cluster (fun () c -> f c) () s

type style = Plain | Selected | Matched

let emit_spans buf line spans (theme : Theme.t) start_col max_width
		sel_range match_ranges =
	let col = ref start_col in
	let grapheme_idx = ref 0 in
	let stopped = ref false in
	let current = ref Plain in
	let in_sel idx =
		match sel_range with
		| None -> false
		| Some (s, e) -> idx >= s && idx < e
	in
	let in_match idx =
		List.exists (fun (s, e) -> idx >= s && idx < e) match_ranges
	in
	let desired_style idx =
		if in_sel idx then Selected
		else if in_match idx then Matched
		else Plain
	in
	let exit_style = function
		| Plain -> ()
		| Selected -> Buffer.add_string buf theme.selection_exit
		| Matched -> Buffer.add_string buf theme.match_exit
	in
	let enter_style style syntax_color =
		match style with
		| Plain -> Buffer.add_string buf syntax_color
		| Selected -> Buffer.add_string buf theme.selection_enter
		| Matched -> Buffer.add_string buf theme.match_enter
	in
	List.iter (fun (span : Syntax.span) ->
		if !stopped then ()
		else begin
			let text = String.sub line span.start span.length in
			let syntax_color = Theme.color_for theme span.kind in
			if !current = Plain then Buffer.add_string buf syntax_color;
			iter_clusters (fun cluster ->
				if !stopped then ()
				else begin
					let want = desired_style !grapheme_idx in
					if want <> !current then begin
						exit_style !current;
						enter_style want syntax_color;
						current := want
					end;
					if cluster = "\t" then begin
						let next = ((!col / tab_width) + 1) * tab_width in
						let spaces = next - !col in
						let allowed = min spaces (max_width - !col) in
						for _ = 1 to allowed do Buffer.add_char buf ' ' done;
						col := !col + allowed;
						if !col >= max_width then stopped := true
					end else begin
						let w = Grapheme.display_width cluster in
						if !col + w > max_width then stopped := true
						else begin
							Buffer.add_string buf cluster;
							col := !col + w
						end
					end;
					incr grapheme_idx
				end
			) text
		end
	) spans;
	exit_style !current;
	Buffer.add_string buf theme.reset

let prefix_display_col prefix =
	let col = ref 0 in
	iter_clusters (fun cluster ->
		if cluster = "\t" then
			col := ((!col / tab_width) + 1) * tab_width
		else
			col := !col + Grapheme.display_width cluster
	) prefix;
	!col

let digits_of n =
	if n <= 0 then 1
	else
		let rec loop n acc = if n = 0 then acc else loop (n / 10) (acc + 1) in
		loop n 0

let gutter_width (state : State.t) =
	let n = Doc.line_count state.doc in
	digits_of (max 1 n) + 1

let find_all_in_line line query =
	if query = "" then []
	else begin
		let ll = String.length line in
		let ql = String.length query in
		if ql > ll then []
		else begin
			let rec loop i acc =
				if i > ll - ql then List.rev acc
				else if String.sub line i ql = query then
					loop (i + ql) (i :: acc)
				else
					loop (i + 1) acc
			in
			loop 0 []
		end
	end

let line_match_ranges (state : State.t) line_text =
	match state.mode with
	| Searching search when search.query <> "" ->
		let byte_offsets = find_all_in_line line_text search.query in
		let query_graphemes = Grapheme.count search.query in
		List.map (fun bo ->
			let gi = Grapheme.index_of_byte line_text bo in
			(gi, gi + query_graphemes)
		) byte_offsets
	| _ -> []

let line_selection_range (state : State.t) line =
	match state.mark with
	| None -> None
	| Some mark ->
		let (a, b) =
			if (mark.line < state.cursor.line)
				|| (mark.line = state.cursor.line
					&& mark.column <= state.cursor.column)
			then (mark, state.cursor)
			else (state.cursor, mark)
		in
		if line < a.line || line > b.line then None
		else
			let start_col = if line = a.line then a.column else 0 in
			let end_col = if line = b.line then b.column else max_int in
			Some (start_col, end_col)

let initial_syntax_state (state : State.t) (ui : Ui.t) lang =
	let rec scan i st =
		if i >= ui.top_line then st
		else
			let line = Option.value (Doc.get_line i state.doc) ~default:"" in
			let _, st' = Syntax.tokenize_line line st lang in
			scan (i + 1) st'
	in
	scan 0 Syntax.Normal

let draw_content buf (state : State.t) (ui : Ui.t) (theme : Theme.t) =
	let lang = Syntax.language_of_filename state.filename in
	let gutter = gutter_width state in
	let content_max = max 0 (ui.cols - gutter) in
	let ch = Ui.content_rows ui in
	let syntax_state = ref (initial_syntax_state state ui lang) in
	for screen_row = 0 to ch - 1 do
		let doc_row = ui.top_line + screen_row in
		move_cursor buf screen_row 0;
		clear_line buf;
		match Doc.get_line doc_row state.doc with
		| Some line ->
			let num_str =
				Printf.sprintf "%*d " (gutter - 1) (doc_row + 1)
			in
			Buffer.add_string buf "\x1b[90m";
			Buffer.add_string buf num_str;
			Buffer.add_string buf theme.reset;
			let spans, next_state =
				Syntax.tokenize_line line !syntax_state lang
			in
			syntax_state := next_state;
			let sel = line_selection_range state doc_row in
			let matches = line_match_ranges state line in
			emit_spans buf line spans theme 0 content_max sel matches
		| None ->
			Buffer.add_string buf (String.make gutter ' ');
			Buffer.add_char buf '~'
	done

let draw_status_bar buf (state : State.t) (ui : Ui.t) =
	let row = ui.rows - 2 in
	move_cursor buf row 0;
	clear_line buf;
	reverse_video buf;
	let filename =
		match state.filename with
		| Some f -> Filename.basename f
		| None -> "[no name]"
	in
	let dirty_marker = if state.dirty then "*" else "" in
	let left = Printf.sprintf " %s%s " filename dirty_marker in
	let right =
		Printf.sprintf " %d:%d "
			(state.cursor.line + 1) (state.cursor.column + 1)
	in
	Buffer.add_string buf left;
	let padding =
		ui.cols - Grapheme.display_width left - Grapheme.display_width right
	in
	for _ = 1 to padding do Buffer.add_char buf ' ' done;
	if padding >= 0 then Buffer.add_string buf right;
	reset_attrs buf

let draw_message_bar buf (state : State.t) (ui : Ui.t) =
	let row = ui.rows - 1 in
	move_cursor buf row 0;
	clear_line buf;
	match state.mode with
	| Searching search ->
		let prompt = Printf.sprintf "Search: %s" search.query in
		Buffer.add_string buf (Grapheme.truncate_to_width prompt ui.cols)
	| Opening_file of_state ->
		let prompt = Printf.sprintf "Open: %s" of_state.path in
		Buffer.add_string buf (Grapheme.truncate_to_width prompt ui.cols)
	| Edit ->
		match state.message with
		| Some m ->
			Buffer.add_string buf (Grapheme.truncate_to_width m ui.cols)
		| None -> ()

let cursor_display_col (state : State.t) =
	match Doc.get_line state.cursor.line state.doc with
	| None -> 0
	| Some line ->
		let byte_idx = Grapheme.byte_of_index line state.cursor.column in
		let prefix = String.sub line 0 byte_idx in
		prefix_display_col prefix

let frame ?(theme = Theme.default) (state : State.t) (ui : Ui.t) =
	let ui = Ui.adjust_viewport state ui in
	let buf = Buffer.create 4096 in
	hide_cursor buf;
	move_cursor buf 0 0;
	draw_content buf state ui theme;
	draw_status_bar buf state ui;
	draw_message_bar buf state ui;
	(match state.mode with
	| Opening_file of_state ->
		let row = ui.rows - 1 in
		let col = 6 + Grapheme.display_width of_state.path in
		move_cursor buf row col
	| _ ->
		let cursor_row = state.cursor.line - ui.top_line in
		let cursor_col = gutter_width state + cursor_display_col state in
		move_cursor buf cursor_row cursor_col);
	show_cursor buf;
	(ui, Buffer.contents buf)
