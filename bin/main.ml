open Twig

let uchar_to_string u =
	let buf = Buffer.create 4 in
	Uutf.Buffer.add_utf_8 buf u;
	Buffer.contents buf

let line_length (state : State.t) line =
	match Doc.get_line line state.doc with
	| Some l -> Grapheme.count l
	| None -> 0

let line_text (state : State.t) line =
	Option.value (Doc.get_line line state.doc) ~default:""

let command_in_search (event : Input.event) : Command.t option =
	match event with
	| Char u ->
		Some (Search_append (uchar_to_string u))
	| Backspace -> Some Search_backspace
	| Enter -> Some Search_commit
	| Escape -> Some Search_cancel
	| Ctrl 'c' -> Some Search_cancel
	| _ -> None

let command_in_open_file (event : Input.event) : Command.t option =
	match event with
	| Char u ->
		Some (Open_file_append (uchar_to_string u))
	| Backspace -> Some Open_file_backspace
	| Enter -> Some Open_file_commit
	| Escape -> Some Open_file_cancel
	| Ctrl 'c' -> Some Open_file_cancel
	| _ -> None

let command_in_edit (ui : Ui.t) (state : State.t) (event : Input.event)
		: Command.t option =
	let cursor = state.cursor in
	match event with
	| Char u ->
		Some (Insert { at = cursor; text = uchar_to_string u })
	| Enter ->
		Some (Insert { at = cursor; text = "\n" })
	| Tab ->
		Some (Insert { at = cursor; text = "\t" })
	| Backspace ->
		if cursor.column > 0 then
			let start_pos : Position.t =
				{ cursor with column = cursor.column - 1 }
			in
			Some (Delete { start_pos; end_pos = cursor })
		else if cursor.line > 0 then
			let prev_line = cursor.line - 1 in
			let prev_len = line_length state prev_line in
			let start_pos : Position.t =
				{ line = prev_line; column = prev_len }
			in
			Some (Delete { start_pos; end_pos = cursor })
		else None
	| Delete ->
		let len = line_length state cursor.line in
		if cursor.column < len then
			let end_pos : Position.t =
				{ cursor with column = cursor.column + 1 }
			in
			Some (Delete { start_pos = cursor; end_pos })
		else if cursor.line + 1 < Doc.line_count state.doc then
			let end_pos : Position.t =
				{ line = cursor.line + 1; column = 0 }
			in
			Some (Delete { start_pos = cursor; end_pos })
		else None
	| Arrow Up ->
		Some (Move_cursor { cursor with line = cursor.line - 1 })
	| Arrow Down ->
		Some (Move_cursor { cursor with line = cursor.line + 1 })
	| Arrow Left ->
		if cursor.column > 0 then
			Some (Move_cursor { cursor with column = cursor.column - 1 })
		else if cursor.line > 0 then
			let prev_len = line_length state (cursor.line - 1) in
			Some (Move_cursor { line = cursor.line - 1; column = prev_len })
		else None
	| Arrow Right ->
		let len = line_length state cursor.line in
		if cursor.column < len then
			Some (Move_cursor { cursor with column = cursor.column + 1 })
		else if cursor.line + 1 < Doc.line_count state.doc then
			Some (Move_cursor { line = cursor.line + 1; column = 0 })
		else None
	| Shift_arrow Up ->
		Some (Extend_cursor { cursor with line = cursor.line - 1 })
	| Shift_arrow Down ->
		Some (Extend_cursor { cursor with line = cursor.line + 1 })
	| Shift_arrow Left ->
		if cursor.column > 0 then
			Some (Extend_cursor { cursor with column = cursor.column - 1 })
		else if cursor.line > 0 then
			let prev_len = line_length state (cursor.line - 1) in
			Some (Extend_cursor { line = cursor.line - 1; column = prev_len })
		else None
	| Shift_arrow Right ->
		let len = line_length state cursor.line in
		if cursor.column < len then
			Some (Extend_cursor { cursor with column = cursor.column + 1 })
		else if cursor.line + 1 < Doc.line_count state.doc then
			Some (Extend_cursor { line = cursor.line + 1; column = 0 })
		else None
	| Home ->
		Some (Move_cursor { cursor with column = 0 })
	| End ->
		let len = line_length state cursor.line in
		Some (Move_cursor { cursor with column = len })
	| Page_up ->
		let page = Ui.content_rows ui in
		Some (Move_cursor { cursor with line = max 0 (cursor.line - page) })
	| Page_down ->
		let page = Ui.content_rows ui in
		Some (Move_cursor { cursor with line = cursor.line + page })
	| Word_left ->
		if cursor.column > 0 then
			let text = line_text state cursor.line in
			let col = Grapheme.prev_word_start text cursor.column in
			Some (Move_cursor { cursor with column = col })
		else if cursor.line > 0 then
			let prev_len = line_length state (cursor.line - 1) in
			Some (Move_cursor { line = cursor.line - 1; column = prev_len })
		else None
	| Word_right ->
		let len = line_length state cursor.line in
		if cursor.column < len then
			let text = line_text state cursor.line in
			let col = Grapheme.next_word_start text cursor.column in
			Some (Move_cursor { cursor with column = col })
		else if cursor.line + 1 < Doc.line_count state.doc then
			Some (Move_cursor { line = cursor.line + 1; column = 0 })
		else None
	| Doc_home ->
		Some (Move_cursor { line = 0; column = 0 })
	| Doc_end ->
		let n = Doc.line_count state.doc in
		if n = 0 then Some (Move_cursor { line = 0; column = 0 })
		else
			let last_line = n - 1 in
			let len = line_length state last_line in
			Some (Move_cursor { line = last_line; column = len })
	| Ctrl 'q' -> Some Quit
	| Ctrl 's' -> Some Save
	| Ctrl 'z' -> Some Undo
	| Ctrl 'y' -> Some Redo
	| Ctrl 'f' -> Some Search_start
	| Ctrl 'n' -> Some Search_next
	| Ctrl 'o' -> Some Open_file_start
	| Ctrl 'g' -> Some Set_mark
	| Ctrl 'c' -> Some Copy
	| Ctrl 'x' -> Some Cut
	| Ctrl 'v' -> Some Paste
	| Eof -> Some Quit
	| Escape -> Some Clear_mark
	| Ctrl _ | Unknown -> None

let command_of_event ui (state : State.t) event =
	match state.mode with
	| Searching _ -> command_in_search event
	| Opening_file _ -> command_in_open_file event
	| Edit -> command_in_edit ui state event

let initial_state () =
	if Array.length Sys.argv > 1 then
		State.of_file Sys.argv.(1)
	else
		State.empty

let main_loop (initial_state : State.t) (initial_ui : Ui.t) =
	let state = ref initial_state in
	let ui = ref initial_ui in
	while not (!state).should_quit do
		if Terminal.resize_flag () then begin
			Terminal.clear_resize_flag ();
			let rows, cols = Terminal.get_size () in
			ui := { !ui with rows; cols }
		end;
		let new_ui, frame = Render.frame !state !ui in
		ui := new_ui;
		Terminal.write frame;
		let event = Input.read () in
		match command_of_event !ui !state event with
		| Some cmd ->
			state := State.apply cmd !state;
			(match cmd with
			| Copy | Cut ->
				(match (!state).yank with
				| Some s -> Terminal.write (Clipboard.osc52_set s)
				| None -> ())
			| _ -> ())
		| None -> ()
	done

let () =
	let state = initial_state () in
	let rows, cols = Terminal.get_size () in
	let ui = Ui.make ~rows ~cols in
	Terminal.install_resize_handler ();
	let saved = Terminal.enter_raw_mode () in
	let cleanup () =
		Terminal.write "\x1b[2J\x1b[H\x1b[?25h";
		Terminal.restore saved
	in
	(try
		main_loop state ui;
		cleanup ()
	with e ->
		cleanup ();
		raise e)
