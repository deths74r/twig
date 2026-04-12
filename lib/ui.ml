type t = {
	rows : int;
	cols : int;
	top_line : int;
	wrap : bool;
	show_line_numbers : bool;
}

let make ~rows ~cols =
	{ rows; cols; top_line = 0; wrap = true; show_line_numbers = true }

let content_rows ui = max 1 (ui.rows - 2)
