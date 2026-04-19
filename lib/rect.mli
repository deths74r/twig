(** Rectangle algebra for terminal layout.

    [t] is a half-open rectangle in cell coordinates: rows and cols
    are the HEIGHT and WIDTH; row and col are the TOP-LEFT corner
    (0-indexed from the terminal origin).

    A rectangle with [rows = 0] or [cols = 0] is empty. All operations
    handle empty rectangles without crashing. [split_*] may produce
    empty halves at extreme ratios; [intersect] returns [None] for
    fully disjoint rectangles. *)

type t = {
	row  : int;
	col  : int;
	rows : int;
	cols : int;
}

val make : row:int -> col:int -> rows:int -> cols:int -> t
(** [make ~row ~col ~rows ~cols] clamps negative dims to 0. *)

val is_empty : t -> bool

val split_h : t -> ratio:float -> t * t
(** Horizontal split (dividing line is horizontal → top/bottom halves).
    [ratio] is the top share, clamped to [0.0, 1.0]. *)

val split_v : t -> ratio:float -> t * t
(** Vertical split (dividing line is vertical → left/right halves).
    [ratio] is the left share, clamped to [0.0, 1.0]. *)

val contains : t -> row:int -> col:int -> bool

val intersect : t -> t -> t option
(** [intersect a b] returns the overlapping rectangle, or [None] if
    [a] and [b] are disjoint. *)

val shrink : t -> top:int -> bottom:int -> left:int -> right:int -> t
(** Inset a rectangle by the given margins. Negative insets clamp to
    0; insets that would produce a negative dimension clamp the
    resulting rectangle to empty. *)
