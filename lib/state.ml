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

type mode =
	| Edit
	| Searching of search
	| Opening_file of open_file

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

let apply cmd t =
	let t = { t with message = None } in
	match cmd with
	| Command.Insert { at; text } ->
		let continue_burst =
			match t.burst with
			| Some p -> Position.equal p at
			| None -> false
		in
		let undo =
			if continue_burst then t.undo
			else snapshot_of t :: t.undo
		in
		let doc = Doc.insert_at ~line:at.line ~column:at.column text t.doc in
		let cursor = clamp_cursor doc (cursor_after_insert at text) in
		{ t with
			doc;
			cursor;
			dirty = true;
			undo;
			redo = [];
			burst = Some cursor;
			mark = None;
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
		| Edit | Opening_file _ -> t
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
		| Edit | Opening_file _ -> t
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
		| Edit | Opening_file _ -> t
		| Searching search ->
			{ t with
				mode = Edit;
				last_search =
					if search.query = "" then t.last_search
					else Some search.query;
			})
	| Command.Search_cancel ->
		(match t.mode with
		| Edit | Opening_file _ -> t
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
