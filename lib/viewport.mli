(** Viewport — a visible window into a Buf.

    Two concerns live here:

    - Geometry ([t]): the rectangle of cells currently visible, plus
      the source line at which the visible region begins and whether
      long lines are wrapped or horizontally scrolled. Used by
      Layout to assign a viewport to each pane leaf, and by
      applications to compute cursor/screen mappings.

    - Wrap math ([segment] and friends): pure functions for splitting
      a source line into display segments that fit within a given
      column width. Used by renderers and applications that need
      wrap-aware cursor positioning. No dependency on editor state,
      themes, or syntax. *)

(** {1 Visible window} *)

type t = {
	top_line : int;
	rows     : int;
	cols     : int;
	wrap     : bool;
}

val make : rows:int -> cols:int -> t
(** [make ~rows ~cols] constructs a viewport at the top of the
    document with wrapping enabled. *)

val scroll_to : t -> line:int -> t
(** [scroll_to vp ~line] returns [vp] with its [top_line] set to
    [max 0 line]. Callers that know the document height should
    additionally clamp to that. *)

(** {1 Wrap math} *)

val tab_width : int

val iter_clusters : (string -> unit) -> string -> unit
(** Walk [s] as grapheme clusters, calling [f] on each. *)

type segment = {
	start_gi : int;
	end_gi   : int;
}

val wrap_line : string -> int -> segment list
(** Split a line into wrap segments, each fitting within [max_width]
    display columns. Returns a single segment covering the whole
    line if [max_width <= 0] or the line is empty. *)

val find_segment_index : segment list -> int -> int
(** Which segment index a grapheme position falls in. *)

val cursor_col_in_segment : string -> segment -> int -> int
(** Display column of a grapheme within a segment. *)

val grapheme_at_col_in_segment : string -> segment -> int -> int
(** Reverse mapping: display column to grapheme index within a
    segment. Used for vertical cursor movement across wrapped
    lines. *)
