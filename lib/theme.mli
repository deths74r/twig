(** Theme — structured terminal styles with a TOML loader.

    Three layers. [chrome] is frame colors (status bar, pane titles,
    borders, tabs, warnings, errors). [markdown] is the rendering
    vocabulary for the markdown tokenizer. [syntax] is code-token
    styling; twig-edit consumes this section today, Phase 4c does
    not.

    All styles have a default (plain) value, so a theme file can
    omit any field and fall back to the engine default. *)

(** {1 Colors and styles} *)

type color =
	| Default
	| Ansi of int                  (** 0-15: 0-7 normal, 8-15 bright *)
	| Ansi256 of int               (** 0-255 *)
	| Rgb of int * int * int       (** 0-255 each *)

type style = {
	fg : color;
	bg : color;
	bold : bool;
	dim : bool;
	italic : bool;
	underline : bool;
	strikethrough : bool;
}

val plain : style
(** All flags false, fg and bg [Default]. *)

val style_to_ansi : style -> string
(** Emit the CSI-SGR escape sequence for [style]. Returns the empty
    string if [style = plain]. Does NOT emit the reset at the end;
    callers emit [reset] after styled content. *)

val reset : string
(** [\x1b[0m] — the SGR reset. *)

(** {1 Theme shape} *)

type chrome = {
	status            : style;
	title_focused     : style;
	title_unfocused   : style;
	border_focused    : style;
	tab_active        : style;
	tab_inactive      : style;
	warning           : style;
	error             : style;
	rule              : style;
		(** Horizontal-rule chrome between chrome regions (e.g.
		    LMI's input-pane top/bottom rules). Themes pick a dim
		    color distinct from status / title so rules read as
		    structural separators rather than content. *)
}

type markdown = {
	heading          : style array;   (** 6 elements, h1..h6 *)
	code_block       : style;
	inline_code      : style;
	link             : style;
	bold             : style;
	italic           : style;
	strikethrough    : style;
	blockquote       : style;
	list_marker      : style;
	hr               : style;
	table_border     : style;
	table_header     : style;
}

type syntax = {
	text             : style;
	keyword          : style;
	type_kw          : style;
	string_lit       : style;
	char_lit         : style;
	number           : style;
	comment          : style;
	preproc          : style;
	(* Editor chrome kept here so twig-edit can migrate without losing
	   its current capabilities. *)
	selection        : style;
	match_highlight  : style;
	bracket_colors   : style array;   (** 4 elements *)
	bracket_match    : style;
	gutter_added     : style;
	gutter_modified  : style;
}

type t = {
	chrome   : chrome;
	markdown : markdown;
	syntax   : syntax;
}

val default : t
(** The bundled default theme. Every style field has a concrete
    value. Used as a fallback for fields missing from a loaded
    TOML file, and as the theme when no file is available. *)

(** {1 Loading} *)

val parse_color : string -> (color, string) result
(** Parse a color string:

    - ["default"] or ["-"] → [Default]
    - ["0".."15"] → [Ansi n]
    - ["16".."255"] → [Ansi256 n]
    - ["#rrggbb"] (6 hex digits) → [Rgb]
    - named colors: ["black"], ["red"], ["green"], ["yellow"],
      ["blue"], ["magenta"], ["cyan"], ["white"],
      and their ["bright_*"] variants → [Ansi]
*)

val load : path:string -> (t, [`Parse_error of string]) result
(** Parse a TOML theme file. Missing fields fall back to [default].
    Returns [Parse_error msg] on IO error, syntax error, or color
    parse error with a message citing the offending key. *)

val load_string : string -> (t, [`Parse_error of string]) result
(** Same as [load] but reads from an in-memory string. Useful for
    tests and for embedded default themes. *)
