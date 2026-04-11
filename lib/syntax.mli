type token =
	| Text
	| Keyword
	| Type_kw
	| String_lit
	| Char_lit
	| Number
	| Comment
	| Preproc

type state =
	| Normal
	| In_block_comment of int

type span = {
	start : int;
	length : int;
	kind : token;
}

type language =
	| Plain
	| C
	| Ocaml
	| Dune
	| Opam

val language_of_filename : string option -> language

val tokenize_line : string -> state -> language -> span list * state
