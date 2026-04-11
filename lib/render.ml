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

let emit_spans buf line spans (theme : Theme.t) max_width =
	let acc = ref 0 in
	let stopped = ref false in
	List.iter (fun (span : Syntax.span) ->
		if !stopped then ()
		else begin
			let text = String.sub line span.start span.length in
			let remaining = max_width - !acc in
			let truncated = Grapheme.truncate_to_width text remaining in
			Buffer.add_string buf (Theme.color_for theme span.kind);
			Buffer.add_string buf truncated;
			acc := !acc + Grapheme.display_width truncated;
			if !acc >= max_width then stopped := true
		end
	) spans;
	Buffer.add_string buf theme.reset

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
	let ch = Ui.content_rows ui in
	let syntax_state = ref (initial_syntax_state state ui lang) in
	for screen_row = 0 to ch - 1 do
		let doc_row = ui.top_line + screen_row in
		move_cursor buf screen_row 0;
		clear_line buf;
		match Doc.get_line doc_row state.doc with
		| Some line ->
			let spans, next_state =
				Syntax.tokenize_line line !syntax_state lang
			in
			syntax_state := next_state;
			emit_spans buf line spans theme ui.cols
		| None ->
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
		Grapheme.display_width prefix

let frame ?(theme = Theme.default) (state : State.t) (ui : Ui.t) =
	let ui = Ui.adjust_viewport state ui in
	let buf = Buffer.create 4096 in
	hide_cursor buf;
	move_cursor buf 0 0;
	draw_content buf state ui theme;
	draw_status_bar buf state ui;
	draw_message_bar buf state ui;
	let cursor_row = state.cursor.line - ui.top_line in
	let cursor_col = cursor_display_col state in
	move_cursor buf cursor_row cursor_col;
	show_cursor buf;
	(ui, Buffer.contents buf)
