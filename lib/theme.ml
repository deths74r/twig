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

let default = {
	keyword    = "\x1b[1;35m";
	type_kw    = "\x1b[36m";
	string_lit = "\x1b[32m";
	char_lit   = "\x1b[32m";
	number     = "\x1b[33m";
	comment    = "\x1b[90m";
	preproc    = "\x1b[31m";
	text       = "\x1b[39m";
	reset      = "\x1b[0m";
	selection_enter = "\x1b[48;5;24m\x1b[38;5;231m";
	selection_exit  = "\x1b[49m\x1b[39m";
	match_enter     = "\x1b[48;5;226m\x1b[38;5;16m";
	match_exit      = "\x1b[49m\x1b[39m";
}

let color_for (theme : t) (token : Syntax.token) =
	match token with
	| Text -> theme.text
	| Keyword -> theme.keyword
	| Type_kw -> theme.type_kw
	| String_lit -> theme.string_lit
	| Char_lit -> theme.char_lit
	| Number -> theme.number
	| Comment -> theme.comment
	| Preproc -> theme.preproc
