type t = {
	rows : int;
	cols : int;
	top_line : int;
}

val make : rows:int -> cols:int -> t

val content_rows : t -> int

val adjust_viewport : State.t -> t -> t
