type saved_state

val enter_raw_mode : unit -> saved_state

val restore : saved_state -> unit

val enable_kitty_keyboard : unit -> unit

val disable_kitty_keyboard : unit -> unit

val get_size : unit -> int * int

val write : string -> unit
(** Low-level unclipped write at the current cursor position. *)

val install_resize_handler : unit -> unit

val resize_flag : unit -> bool

val clear_resize_flag : unit -> unit

(** {1 Region-scoped positional writes}

    [write_at] and [clear_rect] respect a clip rectangle set by the
    innermost enclosing [with_clip]. Writes outside the clip are
    dropped; writes that partially overlap are truncated.

    String clipping treats each byte as a single cell column. This
    is correct for ASCII chrome (borders, titles, padding spaces)
    and for pre-wrapped span-rendered content. Callers emitting
    multi-byte graphemes or ANSI escapes must account for display
    width themselves. *)

val move : row:int -> col:int -> unit
(** Move the cursor to 0-indexed (row, col). Does not clip. *)

val write_at : row:int -> col:int -> string -> unit
(** Position cursor and write, clipped to the current [with_clip]
    rectangle if one is active. No-op if the position is outside
    the clip. Does not interpret newlines. *)

val clear_rect : Rect.t -> unit
(** Fill a rectangle with spaces, clipped to the current clip. *)

val with_clip : Rect.t -> (unit -> unit) -> unit
(** [with_clip r f] runs [f ()] with [r] intersected into the
    current clip. The clip is restored on exit (including on
    exception). *)

val clip_write :
	clip:Rect.t ->
	row:int ->
	col:int ->
	string ->
	(int * int * string) option
(** Pure clipping logic exposed for testing. Returns the
    (row, col, substring) to actually emit, or [None] if the
    write is entirely outside [clip]. *)
