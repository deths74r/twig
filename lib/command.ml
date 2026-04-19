type t =
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
	| Open_file_start
	| Open_file_append of string
	| Open_file_backspace
	| Open_file_commit
	| Open_file_cancel
	| Toggle_wrap
	| Toggle_line_numbers
	| Toggle_diff_markers
	| Enter_command_chord
	| Enter_command_prompt of string
	| Command_input of string
	| Command_backspace
	| Command_execute
	| Command_cancel

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
	| Insert_newline | Indent_block | Outdent_block
	| Move_cursor _ | Extend_cursor _ | Set_mark | Clear_mark
	| Copy | Cut | Paste
	| Save | Quit | Undo | Redo
	| Search_start | Search_append _ | Search_backspace
	| Search_commit | Search_cancel | Search_next
	| Open_file_start | Open_file_append _ | Open_file_backspace
	| Open_file_commit | Open_file_cancel
	| Toggle_wrap | Toggle_line_numbers | Toggle_diff_markers
	| Enter_command_chord | Enter_command_prompt _ | Command_input _
	| Command_backspace | Command_execute | Command_cancel -> doc
