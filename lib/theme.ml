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
