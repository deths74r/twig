type snapshot = {
	snap_doc : Doc.t;
	snap_cursor : Position.t;
}

type search = {
	query : string;
	origin : Position.t;
}

type open_file = {
	path : string;
}

type command_prompt = {
	input : string;
	preview_line : int option;
}

type mode =
	| Edit
	| Searching of search
	| Opening_file of open_file
	| Command_chord
	| Command_prompt of command_prompt

type t = {
	doc : Doc.t;
	cursor : Position.t;
	filename : string option;
	dirty : bool;
	should_quit : bool;
	message : string option;
	burst : Position.t option;
	mode : mode;
	last_search : string option;
	mark : Position.t option;
	yank : string option;
	theme_name : string;
	undo : snapshot list;
	redo : snapshot list;
}

val empty : t

val of_file : string -> t

val apply : Command.t -> t -> t
