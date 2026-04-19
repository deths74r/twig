type snapshot = Buf.snapshot = {
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
	theme_name : string;
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
	theme_name = "default";
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

(* ------------------------------------------------------------------ *)
(* Buffer ↔ State conversion for delegation                          *)
(* ------------------------------------------------------------------ *)

let buffer_of (t : t) : Buf.t = {
	doc = t.doc;
	cursor = t.cursor;
	mark = t.mark;
	yank = t.yank;
	burst = t.burst;
	undo = t.undo;
	redo = t.redo;
	message = None;
}

let update_from_buffer (t : t) (buf : Buf.t) : t = {
	t with
	doc = buf.doc;
	cursor = buf.cursor;
	mark = buf.mark;
	yank = buf.yank;
	burst = buf.burst;
	undo = buf.undo;
	redo = buf.redo;
	message = buf.message;
	dirty = if buf.doc != t.doc then true else t.dirty;
}

(** Convert a Command.t to a Buf.command if it's a buffer
    operation. Returns None for editor-specific commands. *)
let to_buffer_command (cmd : Command.t) : Buf.command option =
	match cmd with
	| Command.Insert { at; text } -> Some (Buf.Insert { at; text })
	| Command.Insert_newline -> Some Buf.Insert_newline
	| Command.Delete { start_pos; end_pos } ->
		Some (Buf.Delete { start_pos; end_pos })
	| Command.Indent_block -> Some Buf.Indent_block
	| Command.Outdent_block -> Some Buf.Outdent_block
	| Command.Move_cursor p -> Some (Buf.Move_cursor p)
	| Command.Extend_cursor p -> Some (Buf.Extend_cursor p)
	| Command.Set_mark -> Some Buf.Set_mark
	| Command.Clear_mark -> Some Buf.Clear_mark
	| Command.Copy -> Some Buf.Copy
	| Command.Cut -> Some Buf.Cut
	| Command.Paste -> Some Buf.Paste
	| Command.Undo -> Some Buf.Undo
	| Command.Redo -> Some Buf.Redo
	| _ -> None

(* ------------------------------------------------------------------ *)
(* Editor-specific helpers                                            *)
(* ------------------------------------------------------------------ *)

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
				doc = Doc.clear_markers t.doc;
				message = Some (Printf.sprintf "saved %s" path);
			}
		with e ->
			{ t with
				message = Some ("save failed: " ^ Printexc.to_string e);
			}

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

(* ------------------------------------------------------------------ *)
(* Apply: delegates buffer commands, handles editor commands          *)
(* ------------------------------------------------------------------ *)

let apply cmd t =
	(* Try buffer command first *)
	match to_buffer_command cmd with
	| Some buf_cmd ->
		let buf = Buf.apply buf_cmd (buffer_of t) in
		update_from_buffer t buf
	| None ->
		(* Editor-specific commands *)
		let t = { t with message = None } in
		match cmd with
		| Command.Save -> { (save t) with burst = None }
		| Command.Quit -> { t with should_quit = true }
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
		| Command.Toggle_diff_markers -> t
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
					| ("w" | "save") :: "as" :: path_parts ->
						let path = String.concat " " path_parts in
						if path = "" then
							{ t with message = Some "save as: no path" }
						else begin
							let t = { t with filename = Some path } in
							save t
						end
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
								cursor = Buf.clamp_cursor t.doc
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
								cursor = Buf.clamp_cursor t.doc
									{ Position.line = n - 1; column = 0 };
								burst = None;
								mark = None;
							}
					| "replace" :: rest ->
						let split_at_with words =
							let rec loop left = function
								| [] -> None
								| "with" :: r ->
									Some (
										String.concat " " (List.rev left),
										String.concat " " r
									)
								| w :: r -> loop (w :: left) r
							in
							loop [] words
						in
						(match split_at_with rest with
						| None | Some ("", _) | Some (_, "") ->
							{ t with
								message = Some "usage: replace <old> with <new>";
							}
						| Some (old_text, new_text) ->
							let undo = { snap_doc = t.doc; snap_cursor = t.cursor } :: t.undo in
							let total = ref 0 in
							let doc = ref t.doc in
							for i = 0 to Doc.line_count t.doc - 1 do
								match Doc.get_line i !doc with
								| None -> ()
								| Some line ->
									let ol = String.length old_text in
									let buf = Stdlib.Buffer.create (String.length line) in
									let j = ref 0 in
									let cnt = ref 0 in
									let ll = String.length line in
									while !j <= ll - ol do
										if String.sub line !j ol = old_text then begin
											Stdlib.Buffer.add_string buf new_text;
											j := !j + ol;
											incr cnt
										end else begin
											Stdlib.Buffer.add_char buf line.[!j];
											incr j
										end
									done;
									while !j < ll do
										Stdlib.Buffer.add_char buf line.[!j];
										incr j
									done;
									if !cnt > 0 then begin
										doc := Doc.replace_line i
											(Stdlib.Buffer.contents buf) !doc;
										total := !total + !cnt
									end
							done;
							if !total = 0 then
								{ t with message = Some "no matches found" }
							else
								{ t with
									doc = !doc;
									dirty = true;
									undo;
									redo = [];
									burst = None;
									message = Some
										(Printf.sprintf "replaced %d occurrence%s"
											!total
											(if !total = 1 then "" else "s"));
								})
					| "dup" :: rest ->
						let n =
							match rest with
							| n_s :: _ ->
								(match int_of_string_opt n_s with
								| Some n -> max 1 n
								| None -> 1)
							| [] -> 1
						in
						let line = t.cursor.line in
						let line_text =
							Option.value
								(Doc.get_line line t.doc) ~default:""
						in
						let undo = { snap_doc = t.doc; snap_cursor = t.cursor } :: t.undo in
						let doc = ref t.doc in
						for i = 1 to n do
							doc := Doc.insert_line (line + i) line_text !doc
						done;
						{ t with
							doc = !doc;
							cursor = Buf.clamp_cursor !doc
								{ t.cursor with line = line + n };
							dirty = true;
							undo;
							redo = [];
							burst = None;
							message = Some
								(Printf.sprintf "duplicated %d line%s"
									n (if n = 1 then "" else "s"));
						}
					| ["theme"] ->
						let current = t.theme_name in
						let names = Theme.names in
						let rec next = function
							| [] -> List.hd names
							| [_] -> List.hd names
							| a :: b :: _ when a = current -> b
							| _ :: rest -> next rest
						in
						let name = next names in
						{ t with
							theme_name = name;
							message = Some
								(Printf.sprintf "theme: %s" name);
						}
					| "theme" :: name :: _ ->
						let name = String.lowercase_ascii name in
						(match Theme.by_name name with
						| Some _ ->
							{ t with
								theme_name = name;
								message = Some
									(Printf.sprintf "theme: %s" name);
							}
						| None ->
							{ t with
								message = Some
									(Printf.sprintf
										"unknown theme (available: %s)"
										(String.concat ", " Theme.names));
							})
					| ["help"] ->
						{ t with
							message = Some
								"Ctrl-F:find Ctrl-S:save Ctrl-Z:undo \
								 Ctrl-Y:redo Ctrl-O:open \
								 ESC/Shift+Space:commands";
						}
					| ["wrap"] ->
						{ t with message = Some "use Alt-Z to toggle wrap" }
					| ["numbers"] ->
						{ t with message = Some "use Alt-L to toggle line numbers" }
					| ["diff"] | ["diffs"] ->
						{ t with message = Some "use Alt-D to toggle diff markers" }
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
		(* Buffer commands handled above — this catch-all is
		   unreachable but silences the exhaustiveness warning
		   since to_buffer_command returns Some for all of them. *)
		| Command.Insert _ | Command.Insert_newline
		| Command.Delete _ | Command.Indent_block
		| Command.Outdent_block | Command.Move_cursor _
		| Command.Extend_cursor _ | Command.Set_mark
		| Command.Clear_mark | Command.Copy | Command.Cut
		| Command.Paste | Command.Undo | Command.Redo ->
			(* Already handled by to_buffer_command *)
			t
