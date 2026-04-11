type t = {
	rows : int;
	cols : int;
	top_line : int;
}

let make ~rows ~cols = { rows; cols; top_line = 0 }

let content_rows ui = max 1 (ui.rows - 2)

let adjust_viewport (state : State.t) ui =
	let ch = content_rows ui in
	let cursor_line = state.cursor.line in
	let top =
		if cursor_line < ui.top_line then cursor_line
		else if cursor_line >= ui.top_line + ch then cursor_line - ch + 1
		else ui.top_line
	in
	{ ui with top_line = max 0 top }
