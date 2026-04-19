type t = {
	rows : int;
	cols : int;
	top_line : int;
	wrap : bool;
	show_line_numbers : bool;
	show_diff_markers : bool;
}

val make : rows:int -> cols:int -> t

val content_rows : t -> int
