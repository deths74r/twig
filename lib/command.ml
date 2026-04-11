type t =
	| Insert of { at : Position.t; text : string }
	| Delete of { start_pos : Position.t; end_pos : Position.t }
	| Move_cursor of Position.t
	| Save
	| Quit
	| Undo
	| Redo
	| Search_start
	| Search_append of string
	| Search_backspace
	| Search_commit
	| Search_cancel
	| Search_next

let apply_to_doc cmd doc =
	match cmd with
	| Insert { at; text } ->
		Doc.insert_at ~line:at.line ~column:at.column text doc
	| Delete { start_pos; end_pos } ->
		Doc.delete_range
			~start_line:start_pos.line
			~start_col:start_pos.column
			~end_line:end_pos.line
			~end_col:end_pos.column
			doc
	| Move_cursor _ | Save | Quit | Undo | Redo
	| Search_start | Search_append _ | Search_backspace
	| Search_commit | Search_cancel | Search_next -> doc
