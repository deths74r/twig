type snapshot = {
	snap_doc : Doc.t;
	snap_cursor : Position.t;
}

type search = {
	query : string;
	origin : Position.t;
}

type open_file = {
	path : string;
}

type command_prompt = {
	input : string;
	preview_line : int option;
}

type mode =
	| Edit
	| Searching of search
	| Opening_file of open_file
	| Command_chord
	| Command_prompt of command_prompt

type t = {
	doc : Doc.t;
	cursor : Position.t;
	filename : string option;
	dirty : bool;
	should_quit : bool;
	message : string option;
	burst : Position.t option;
	mode : mode;
	last_search : string option;
	mark : Position.t option;
	yank : string option;
	undo : snapshot list;
	redo : snapshot list;
}

let empty = {
	doc = Doc.empty;
	cursor = Position.origin;
	filename = None;
	dirty = false;
	should_quit = false;
	message = None;
	burst = None;
	mode = Edit;
	last_search = None;
	mark = None;
	yank = None;
	undo = [];
	redo = [];
}

let of_file path =
	try
		let ic = open_in path in
		let doc =
			try
				let d = Doc.of_channel ic in
				close_in ic;
				d
			with e ->
				close_in_noerr ic;
				raise e
		in
		{ empty with doc; filename = Some path }
	with Sys_error _ ->
		{ empty with filename = Some path; message = Some "new file" }

let snapshot_of t =
	{ snap_doc = t.doc; snap_cursor = t.cursor }

let restore s t =
	{ t with doc = s.snap_doc; cursor = s.snap_cursor }

let clamp_cursor doc (pos : Position.t) : Position.t =
	let lines = Doc.line_count doc in
	if lines = 0 then Position.origin
	else begin
		let line = max 0 (min pos.line (lines - 1)) in
		let line_text = Option.value (Doc.get_line line doc) ~default:"" in
		let max_col = Grapheme.count line_text in
		let column = max 0 (min pos.column max_col) in
		{ line; column }
	end

let cursor_after_insert (at : Position.t) text : Position.t =
	let newlines =
		String.fold_left
			(fun acc c -> if c = '\n' then acc + 1 else acc)
			0 text
	in
	if newlines = 0 then
		{ at with column = at.column + Grapheme.count text }
	else
		let last_segment =
			match List.rev (String.split_on_char '\n' text) with
			| [] -> ""
			| s :: _ -> s
		in
		{ line = at.line + newlines; column = Grapheme.count last_segment }

let save t =
	match t.filename with
	| None -> { t with message = Some "no filename" }
	| Some path ->
		try
			let dir = Filename.dirname path in
			let base = Filename.basename path in
			let tmp = Filename.concat dir ("." ^ base ^ ".twigtmp") in
			let oc = open_out tmp in
			(try
				output_string oc (Doc.to_string t.doc);
				close_out oc
			with e ->
				close_out_noerr oc;
				(try Sys.remove tmp with _ -> ());
				raise e);
			Sys.rename tmp path;
			{ t with
				dirty = false;
				message = Some (Printf.sprintf "saved %s" path);
			}
		with e ->
			{ t with
				message = Some ("save failed: " ^ Printexc.to_string e);
			}

let consume_selection t =
	match t.mark with
	| None -> t
	| Some m ->
		let cursor = t.cursor in
		let (a, b) =
			if (m.line < cursor.line)
				|| (m.line = cursor.line && m.column <= cursor.column)
			then (m, cursor)
			else (cursor, m)
		in
		if Position.equal a b then { t with mark = None }
		else begin
			let doc =
				Doc.delete_range t.doc
					~start_line:a.line ~start_col:a.column
					~end_line:b.line ~end_col:b.column
			in
			{ t with doc; cursor = a; mark = None }
		end

let parse_goto_preview input t =
	let trimmed = String.trim input in
	let parts = String.split_on_char ' ' trimmed in
	match parts with
	| ("goto" | "g") :: n :: _ ->
		(match int_of_string_opt n with
		| Some line ->
			let line = max 0 (line - 1) in
			if line < Doc.line_count t.doc then Some line else None
		| None -> None)
	| _ -> None

let leading_ws_len s =
	let n = String.length s in
	let rec loop i =
		if i >= n then i
		else match s.[i] with
			| ' ' | '\t' -> loop (i + 1)
			| _ -> i
	in
	loop 0

let apply cmd t =
	let t = { t with message = None } in
	match cmd with
	| Command.Insert { at; text } ->
		let had_mark = t.mark <> None in
		let continue_burst =
			if had_mark then false
			else
				match t.burst with
				| Some p -> Position.equal p at
				| None -> false
		in
		let undo =
			if continue_burst then t.undo
			else snapshot_of t :: t.undo
		in
		let t, effective_at =
			if had_mark then
				let t = consume_selection t in
				(t, t.cursor)
			else
				(t, at)
		in
		let doc =
			Doc.insert_at
				~line:effective_at.line
				~column:effective_at.column
				text t.doc
		in
		let cursor =
			clamp_cursor doc (cursor_after_insert effective_at text)
		in
		{ t with
			doc;
			cursor;
			dirty = true;
			undo;
			redo = [];
			burst = Some cursor;
			mark = None;
		}
	| Command.Insert_newline ->
		let undo = snapshot_of t :: t.undo in
		let t =
			match t.mark with
			| None -> t
			| Some _ -> consume_selection t
		in
		let line_text =
			Option.value (Doc.get_line t.cursor.line t.doc) ~default:""
		in
		let indent_end = leading_ws_len line_text in
		let cursor_byte = Grapheme.byte_of_index line_text t.cursor.column in
		let indent =
			if cursor_byte >= indent_end then String.sub line_text 0 indent_end
			else ""
		in
		let text = "\n" ^ indent in
		let at = t.cursor in
		let doc =
			Doc.insert_at ~line:at.line ~column:at.column text t.doc
		in
		let cursor = clamp_cursor doc (cursor_after_insert at text) in
		{ t with
			doc;
			cursor;
			dirty = true;
			undo;
			redo = [];
			burst = None;
			mark = None;
		}
	| Command.Indent_block ->
		let a, b =
			match t.mark with
			| None -> (t.cursor.line, t.cursor.line)
			| Some m ->
				(min m.line t.cursor.line, max m.line t.cursor.line)
		in
		let doc = ref t.doc in
		for i = a to b do
			doc := Doc.insert_at ~line:i ~column:0 "\t" !doc
		done;
		let shift (pos : Position.t) =
			if pos.line >= a && pos.line <= b then
				{ pos with column = pos.column + 1 }
			else pos
		in
		{ t with
			doc = !doc;
			cursor = shift t.cursor;
			mark = Option.map shift t.mark;
			dirty = true;
			undo = snapshot_of t :: t.undo;
			redo = [];
			burst = None;
		}
	| Command.Outdent_block ->
		let a, b =
			match t.mark with
			| None -> (t.cursor.line, t.cursor.line)
			| Some m ->
				(min m.line t.cursor.line, max m.line t.cursor.line)
		in
		let doc = ref t.doc in
		let removals = Array.make (b - a + 1) 0 in
		for i = a to b do
			match Doc.get_line i !doc with
			| None -> ()
			| Some line ->
				let remove =
					if String.length line > 0 && line.[0] = '\t' then 1
					else begin
						let limit = min 8 (String.length line) in
						let n = ref 0 in
						while !n < limit && line.[!n] = ' ' do incr n done;
						!n
					end
				in
				if remove > 0 then begin
					doc :=
						Doc.delete_range !doc
							~start_line:i ~start_col:0
							~end_line:i ~end_col:remove;
					removals.(i - a) <- remove
				end
		done;
		let shift (pos : Position.t) =
			if pos.line >= a && pos.line <= b then
				let r = removals.(pos.line - a) in
				{ pos with column = max 0 (pos.column - r) }
			else pos
		in
		{ t with
			doc = !doc;
			cursor = shift t.cursor;
			mark = Option.map shift t.mark;
			dirty = true;
			undo = snapshot_of t :: t.undo;
			redo = [];
			burst = None;
		}
	| Command.Delete { start_pos; end_pos } ->
		let undo = snapshot_of t :: t.undo in
		let doc =
			Doc.delete_range
				~start_line:start_pos.line
				~start_col:start_pos.column
				~end_line:end_pos.line
				~end_col:end_pos.column
				t.doc
		in
		{ t with
			doc;
			cursor = clamp_cursor doc start_pos;
			dirty = true;
			undo;
			redo = [];
			burst = None;
			mark = None;
		}
	| Command.Move_cursor pos ->
		{ t with
			cursor = clamp_cursor t.doc pos;
			burst = None;
			mark = None;
		}
	| Command.Extend_cursor pos ->
		let mark =
			match t.mark with
			| Some _ -> t.mark
			| None -> Some t.cursor
		in
		{ t with
			cursor = clamp_cursor t.doc pos;
			burst = None;
			mark;
		}
	| Command.Set_mark ->
		{ t with mark = Some t.cursor; message = Some "mark set" }
	| Command.Clear_mark ->
		{ t with mark = None }
	| Command.Copy ->
		(match t.mark with
		| None -> { t with message = Some "no selection" }
		| Some m ->
			let text = Doc.extract_range t.doc ~start:m ~stop:t.cursor in
			{ t with
				yank = Some text;
				mark = None;
				message = Some "copied";
			})
	| Command.Cut ->
		(match t.mark with
		| None -> { t with message = Some "no selection" }
		| Some m ->
			let text = Doc.extract_range t.doc ~start:m ~stop:t.cursor in
			let (a, b) =
				if (m.line < t.cursor.line)
					|| (m.line = t.cursor.line && m.column <= t.cursor.column)
				then (m, t.cursor)
				else (t.cursor, m)
			in
			let doc =
				Doc.delete_range
					~start_line:a.line ~start_col:a.column
					~end_line:b.line ~end_col:b.column
					t.doc
			in
			{ t with
				doc;
				cursor = clamp_cursor doc a;
				yank = Some text;
				mark = None;
				dirty = true;
				undo = snapshot_of t :: t.undo;
				redo = [];
				burst = None;
				message = Some "cut";
			})
	| Command.Paste ->
		(match t.yank with
		| None -> { t with message = Some "yank buffer empty" }
		| Some text ->
			let undo = snapshot_of t :: t.undo in
			let t =
				match t.mark with
				| None -> t
				| Some _ -> consume_selection t
			in
			let at = t.cursor in
			let doc =
				Doc.insert_at ~line:at.line ~column:at.column text t.doc
			in
			let cursor = clamp_cursor doc (cursor_after_insert at text) in
			{ t with
				doc;
				cursor;
				dirty = true;
				undo;
				redo = [];
				burst = None;
				mark = None;
			})
	| Command.Save -> { (save t) with burst = None }
	| Command.Quit -> { t with should_quit = true }
	| Command.Undo ->
		(match t.undo with
		| [] -> t
		| s :: rest ->
			let redo = snapshot_of t :: t.redo in
			let t = restore s t in
			{ t with
				undo = rest;
				redo;
				dirty = true;
				burst = None;
				mark = None;
			})
	| Command.Redo ->
		(match t.redo with
		| [] -> t
		| s :: rest ->
			let undo = snapshot_of t :: t.undo in
			let t = restore s t in
			{ t with
				undo;
				redo = rest;
				dirty = true;
				burst = None;
				mark = None;
			})
	| Command.Search_start ->
		{ t with
			mode = Searching { query = ""; origin = t.cursor };
			burst = None;
		}
	| Command.Search_append s ->
		(match t.mode with
		| Edit | Opening_file _ | Command_chord | Command_prompt _ -> t
		| Searching search ->
			let query = search.query ^ s in
			let cursor =
				match Doc.find_forward t.doc ~from:search.origin ~query with
				| Some p -> p
				| None -> search.origin
			in
			{ t with
				mode = Searching { search with query };
				cursor;
			})
	| Command.Search_backspace ->
		(match t.mode with
		| Edit | Opening_file _ | Command_chord | Command_prompt _ -> t
		| Searching search ->
			let qlen = String.length search.query in
			if qlen = 0 then t
			else begin
				let query = String.sub search.query 0 (qlen - 1) in
				let cursor =
					if query = "" then search.origin
					else
						match
							Doc.find_forward t.doc ~from:search.origin ~query
						with
						| Some p -> p
						| None -> search.origin
				in
				{ t with
					mode = Searching { search with query };
					cursor;
				}
			end)
	| Command.Search_commit ->
		(match t.mode with
		| Edit | Opening_file _ | Command_chord | Command_prompt _ -> t
		| Searching search ->
			{ t with
				mode = Edit;
				last_search =
					if search.query = "" then t.last_search
					else Some search.query;
			})
	| Command.Search_cancel ->
		(match t.mode with
		| Edit | Opening_file _ | Command_chord | Command_prompt _ -> t
		| Searching search ->
			{ t with mode = Edit; cursor = search.origin })
	| Command.Search_next ->
		(match t.last_search with
		| None -> t
		| Some query ->
			let from = Doc.advance t.doc t.cursor in
			(match Doc.find_forward t.doc ~from ~query with
			| Some p -> { t with cursor = p; burst = None }
			| None ->
				{ t with message = Some "no more matches" }))
	| Command.Open_file_start ->
		let initial =
			match t.filename with
			| None -> ""
			| Some f ->
				let dir = Filename.dirname f in
				if dir = "." || dir = "" then ""
				else dir ^ "/"
		in
		{ t with mode = Opening_file { path = initial }; burst = None }
	| Command.Open_file_append s ->
		(match t.mode with
		| Opening_file of_state ->
			{ t with mode = Opening_file { path = of_state.path ^ s } }
		| _ -> t)
	| Command.Open_file_backspace ->
		(match t.mode with
		| Opening_file of_state ->
			let plen = String.length of_state.path in
			if plen = 0 then t
			else
				let path = String.sub of_state.path 0 (plen - 1) in
				{ t with mode = Opening_file { path } }
		| _ -> t)
	| Command.Open_file_cancel ->
		(match t.mode with
		| Opening_file _ -> { t with mode = Edit }
		| _ -> t)
	| Command.Toggle_wrap -> t
	| Command.Toggle_line_numbers -> t
	| Command.Enter_command_chord ->
		{ t with mode = Command_chord; burst = None }
	| Command.Enter_command_prompt prefix ->
		{ t with
			mode = Command_prompt { input = prefix; preview_line = None };
			burst = None;
		}
	| Command.Command_input s ->
		(match t.mode with
		| Command_prompt cp ->
			let input = cp.input ^ s in
			let preview_line = parse_goto_preview input t in
			{ t with mode = Command_prompt { input; preview_line } }
		| _ -> t)
	| Command.Command_backspace ->
		(match t.mode with
		| Command_prompt cp ->
			let len = String.length cp.input in
			if len = 0 then { t with mode = Edit }
			else begin
				let input = String.sub cp.input 0 (len - 1) in
				let preview_line = parse_goto_preview input t in
				{ t with mode = Command_prompt { input; preview_line } }
			end
		| _ -> t)
	| Command.Command_cancel ->
		(match t.mode with
		| Command_prompt _ | Command_chord -> { t with mode = Edit }
		| _ -> t)
	| Command.Command_execute ->
		(match t.mode with
		| Command_prompt cp ->
			let input = String.trim cp.input in
			let t = { t with mode = Edit } in
			if input = "" then t
			else begin
				let parts = String.split_on_char ' ' input in
				match parts with
				| ("q" | "quit") :: _ ->
					if t.dirty then
						{ t with message = Some "unsaved changes (use q! to force)" }
					else
						{ t with should_quit = true }
				| ("q!" | "quit!") :: _ ->
					{ t with should_quit = true }
				| ("w" | "save") :: _ ->
					save t
				| ("wq") :: _ ->
					let t = save t in
					{ t with should_quit = true }
				| "goto" :: n :: _ | "g" :: n :: _ ->
					(match int_of_string_opt n with
					| Some line ->
						let line = max 0 (line - 1) in
						{ t with
							cursor = clamp_cursor t.doc
								{ Position.line; column = 0 };
							burst = None;
							mark = None;
						}
					| None ->
						{ t with message = Some "goto: invalid line number" })
				| ["top"] ->
					{ t with
						cursor = Position.origin;
						burst = None;
						mark = None;
					}
				| ["bottom"] | ["bot"] ->
					let n = Doc.line_count t.doc in
					if n = 0 then t
					else
						{ t with
							cursor = clamp_cursor t.doc
								{ Position.line = n - 1; column = 0 };
							burst = None;
							mark = None;
						}
				| ["wrap"] ->
					{ t with message = Some "use Alt-Z to toggle wrap" }
				| ["numbers"] ->
					{ t with message = Some "use Alt-L to toggle line numbers" }
				| _ ->
					{ t with
						message = Some
							(Printf.sprintf "unknown command: %s" input);
					}
			end
		| _ -> t)
	| Command.Open_file_commit ->
		(match t.mode with
		| Opening_file of_state ->
			if of_state.path = "" then
				{ t with mode = Edit; message = Some "no path" }
			else if t.dirty then
				{ t with
					mode = Edit;
					message = Some "unsaved changes — save first";
				}
			else begin
				try of_file of_state.path
				with e ->
					{ t with
						mode = Edit;
						message = Some ("open failed: " ^ Printexc.to_string e);
					}
			end
		| _ -> t)
