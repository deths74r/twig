(** Pane tree for a splittable terminal workspace.

    A [t] is a binary tree of horizontal and vertical splits with
    buffers at the leaves. Exactly one leaf holds focus. Phase
    4a-i exposes construction, split, close, and path-based
    access. Focus-move, resize, equalize, and rendering land in
    4a-ii / 4a-iii.

    Paths address leaves as int lists: [] is the root, [0] is the
    left/top child of the root's split, [0; 1] is the right/bottom
    child of that child, and so on. [0 = left/top], [1 =
    right/bottom]. *)

type dir = Horizontal | Vertical
(** A [Horizontal] split produces top and bottom panes.
    A [Vertical] split produces left and right panes.
    Matches vim convention: [:split] is horizontal,
    [:vsplit] is vertical. *)

type pane = {
	buf      : Buf.t;
	viewport : Viewport.t;
	title    : string option;
}

type tree =
	| Leaf of pane
	| Split of {
		dir   : dir;
		ratio : float;  (** left/top share in [0.0, 1.0] *)
		left  : tree;
		right : tree;
	}

type path = int list

type t = {
	root       : tree;
	focus_path : path;
}

(** {1 Construction} *)

val single : ?title:string -> Buf.t -> unit -> t
(** [single buf ()] returns a one-leaf layout with [buf] at the
    root and focus on it. *)

val split : t -> dir -> ?title:string -> Buf.t -> unit -> t
(** [split t dir buf ()] splits the focused leaf: the leaf at
    [t.focus_path] is replaced by a [Split] whose left/top child
    is the original leaf and right/bottom child is a new leaf
    holding [buf]. Focus moves to the new leaf.

    Default split ratio is 0.5; adjust via [resize] (4a-ii).

    Vim convention: splitting moves focus to the new pane. *)

val close : t -> t option
(** [close t] removes the focused leaf. The parent [Split]
    collapses to its remaining sibling subtree; focus moves
    one level up the path.

    Returns [None] when the focused leaf is the only pane (i.e.
    the root is a [Leaf]) — a layout must have at least one
    pane. *)

(** {1 Leaf access} *)

val iter_leaves : t -> f:(path -> pane -> unit) -> unit
(** Visit every leaf with its path, left-to-right /
    top-to-bottom in the tree. *)

val find_leaf : t -> path:path -> pane option
(** [find_leaf t ~path] returns the pane at [path], or [None]
    if [path] does not address a leaf. *)

val replace_leaf : t -> path:path -> pane -> t
(** [replace_leaf t ~path pane] swaps the leaf at [path] with
    [pane]. Tree structure and focus are preserved. If [path]
    does not address a leaf, the layout is returned unchanged. *)

(** {1 Invariants} *)

val focus : t -> pane option
(** The currently-focused pane, or [None] if the layout is
    somehow in an invalid state (should not happen for layouts
    constructed via this module's operations). *)
