(** Reusable text buffer with cursor, selection, undo/redo.

    The core engine type. Applications wrap this with their own
    state (file tracking, modes, themes). Buffer handles text
    manipulation; it doesn't know about files, syntax, or UI. *)

type snapshot = {
	snap_doc : Doc.t;
	snap_cursor : Position.t;
}

type t = {
	doc : Doc.t;
	cursor : Position.t;
	mark : Position.t option;
	yank : string option;
	burst : Position.t option;
	undo : snapshot list;
	redo : snapshot list;
	message : string option;
}

type command =
	| Insert of { at : Position.t; text : string }
	| Insert_newline
	| Delete of { start_pos : Position.t; end_pos : Position.t }
	| Indent_block
	| Outdent_block
	| Move_cursor of Position.t
	| Extend_cursor of Position.t
	| Set_mark
	| Clear_mark
	| Copy
	| Cut
	| Paste
	| Undo
	| Redo

val empty : t
val of_doc : Doc.t -> t
val apply : command -> t -> t
val clamp_cursor : Doc.t -> Position.t -> Position.t
val cursor_after_insert : Position.t -> string -> Position.t
