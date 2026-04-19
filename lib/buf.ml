(** Reusable text buffer with cursor, selection, undo/redo.

    The core engine type. Applications (editors, TUIs, agent
    interfaces) wrap this with their own state — file tracking,
    modes, themes, etc. Buffer knows about text manipulation;
    it doesn't know about files, syntax, or UI. *)

type snapshot = {
	snap_doc : Doc.t;
	snap_cursor : Position.t;
}

type t = {
	doc : Doc.t;
	cursor : Position.t;
	mark : Position.t option;
	yank : string option;
	burst : Position.t option;
	undo : snapshot list;
	redo : snapshot list;
	message : string option;
}

type command =
	| Insert of { at : Position.t; text : string }
	| Insert_newline
	| Delete of { start_pos : Position.t; end_pos : Position.t }
	| Indent_block
	| Outdent_block
	| Move_cursor of Position.t
	| Extend_cursor of Position.t
	| Set_mark
	| Clear_mark
	| Copy
	| Cut
	| Paste
	| Undo
	| Redo

let empty = {
	doc = Doc.empty;
	cursor = Position.origin;
	mark = None;
	yank = None;
	burst = None;
	undo = [];
	redo = [];
	message = None;
}

let of_doc doc = { empty with doc }

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
	| Insert { at; text } ->
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
			undo;
			redo = [];
			burst = Some cursor;
			mark = None;
		}
	| Insert_newline ->
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
			undo;
			redo = [];
			burst = None;
			mark = None;
		}
	| Indent_block ->
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
			undo = snapshot_of t :: t.undo;
			redo = [];
			burst = None;
		}
	| Outdent_block ->
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
			undo = snapshot_of t :: t.undo;
			redo = [];
			burst = None;
		}
	| Delete { start_pos; end_pos } ->
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
			undo;
			redo = [];
			burst = None;
			mark = None;
		}
	| Move_cursor pos ->
		{ t with
			cursor = clamp_cursor t.doc pos;
			burst = None;
			mark = None;
		}
	| Extend_cursor pos ->
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
	| Set_mark ->
		{ t with mark = Some t.cursor; message = Some "mark set" }
	| Clear_mark ->
		{ t with mark = None }
	| Copy ->
		(match t.mark with
		| None -> { t with message = Some "no selection" }
		| Some m ->
			let text = Doc.extract_range t.doc ~start:m ~stop:t.cursor in
			{ t with
				yank = Some text;
				mark = None;
				message = Some "copied";
			})
	| Cut ->
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
			{
				doc;
				cursor = clamp_cursor doc a;
				yank = Some text;
				mark = None;
				undo = snapshot_of t :: t.undo;
				redo = [];
				burst = None;
				message = Some "cut";
			})
	| Paste ->
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
				undo;
				redo = [];
				burst = None;
				mark = None;
			})
	| Undo ->
		(match t.undo with
		| [] -> t
		| s :: rest ->
			let redo = snapshot_of t :: t.redo in
			let t = restore s t in
			{ t with
				undo = rest;
				redo;
				burst = None;
				mark = None;
			})
	| Redo ->
		(match t.redo with
		| [] -> t
		| s :: rest ->
			let undo = snapshot_of t :: t.undo in
			let t = restore s t in
			{ t with
				undo;
				redo = rest;
				burst = None;
				mark = None;
			})
