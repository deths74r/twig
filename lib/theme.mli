type t = {
	keyword : string;
	type_kw : string;
	string_lit : string;
	char_lit : string;
	number : string;
	comment : string;
	preproc : string;
	text : string;
	reset : string;
	selection_enter : string;
	selection_exit : string;
	match_enter : string;
	match_exit : string;
}

val default : t

val color_for : t -> Syntax.token -> string
