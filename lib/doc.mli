type t

type measure = {
	lines : int;
	bytes : int;
}

val empty : t

val of_string : string -> t

val of_channel : in_channel -> t

val to_string : t -> string

val measure : t -> measure

val line_count : t -> int

val get_line : int -> t -> string option

val insert_line : int -> string -> t -> t

val delete_line : int -> t -> t

val replace_line : int -> string -> t -> t

val insert_at : line:int -> column:int -> string -> t -> t

val delete_range :
	start_line:int -> start_col:int ->
	end_line:int -> end_col:int ->
	t -> t

val find_forward :
	t -> from:Position.t -> query:string -> Position.t option

val advance : t -> Position.t -> Position.t
