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

type render_mode =
	| Markdown
	| Plain of {
			prefix_first : string;
			prefix_rest  : string;
			prefix_style : Theme.style option;
		}

type pane = {
	buf         : Buf.t;
	viewport    : Viewport.t;
	title       : string option;
	render_mode : render_mode;
	min_rows    : int;
	content_inset_top    : int;
		(** Rows reserved at the top of the content area AFTER the
		    title. The caller draws whatever it wants in those
		    rows; [render_content] just shifts content down. *)
	content_inset_bottom : int;
		(** Symmetric bottom inset. *)
	content_inset_left   : int;
		(** Columns reserved at the left of the content area.
		    Content-line rendering starts at [rect.col + inset_left]
		    with wrap width reduced accordingly. Title and the
		    top/bottom reserved rows are unaffected (those remain
		    full-width; chrome drawing there is the caller's job). *)
	content_inset_right  : int;
		(** Symmetric right inset. *)
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

val single : ?title:string -> ?render_mode:render_mode -> ?min_rows:int ->
	?content_inset_top:int -> ?content_inset_bottom:int ->
	?content_inset_left:int -> ?content_inset_right:int ->
	Buf.t -> unit -> t
(** [single buf ()] returns a one-leaf layout with [buf] at the
    root and focus on it. *)

val split : t -> dir -> ?title:string -> ?render_mode:render_mode ->
	?min_rows:int ->
	?content_inset_top:int -> ?content_inset_bottom:int ->
	?content_inset_left:int -> ?content_inset_right:int ->
	Buf.t -> unit -> t
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

(** {1 Focus} *)

val leaf_rects : t -> Rect.t -> (path * pane * Rect.t) list
(** Compute the screen rectangle for every leaf pane given
    the overall layout rect. Used to sync viewport dimensions
    with actual screen size. *)

val swap : t -> rect:Rect.t ->
	[ `Left | `Right | `Up | `Down ] -> t
(** Swap the focused pane with its nearest neighbor in the
    given direction. Focus follows the swapped pane. *)

val fullscreen : t -> t
(** Toggle fullscreen. Entering: stash tree, focused pane
    fills root. Exiting: restore stashed tree with updated
    pane contents. *)

val focus : t -> pane option
(** The currently-focused pane, or [None] if the layout is
    somehow in an invalid state (should not happen for layouts
    constructed via this module's operations). *)

val focus_move :
	t ->
	rect:Rect.t ->
	[ `Left | `Right | `Up | `Down ] ->
	t
(** Move focus to the nearest leaf in the given direction.
    [rect] is the rectangle covering the whole layout — the
    caller provides it so focus math can be geometric rather
    than structural (Ctrl-W in vim is structural, which loses
    on non-trivial trees; this is closer to i3's directional
    focus).

    "Nearest" = candidate whose center is closest to the
    focused pane's center by Manhattan distance. A candidate
    must lie entirely in the requested direction from the
    focused pane's bounding edges.

    No candidate in that direction → returned unchanged. *)

(** {1 Resize} *)

val resize : t -> rect:Rect.t -> delta:float -> t
(** Adjust the ratio of the nearest [Split] ancestor of the
    focused leaf. [delta] is signed and expresses the desired
    size change for the FOCUSED pane: positive grows it,
    negative shrinks it.

    [rect] is the whole-layout rectangle (same one passed to
    [render]). Used to compute the split's actual pixel
    dimensions so the clamp enforces a MINIMUM 20-COLUMN
    (for vertical splits) or 20-ROW (for horizontal splits)
    leaf size, not just a ratio floor. Per spec 18_tui.md §4,
    §6: "leaves clamp to 20-column minimum."

    A ratio that would make either side smaller than 20 cells
    is rejected — the resize degrades to either the minimum
    feasible or a no-op. If the focused leaf is the root (no
    ancestor split), the layout is returned unchanged. *)

val equalize : t -> t
(** Set every [Split.ratio] in the tree to 0.5. Focus and tree
    structure are preserved. *)

(** {1 Render} *)

val render :
	?show_cursor_cell:bool ->
	t -> rect:Rect.t -> theme:Theme.t -> (int * int) option
(** Render the layout to the terminal via [Terminal.write_at]
    under [Terminal.with_clip] (spec 18_tui.md §9). Each leaf
    is drawn inside its computed sub-rect:

    - Title bar (when [pane.title = Some]) at the top row,
      styled via [theme.chrome.title_focused] (focused pane)
      or [theme.chrome.title_unfocused].
    - Content rows show [pane.buf.doc] starting at
      [pane.viewport.top_line], with each line run through
      [Markdown.tokenize_line] so headings, emphasis, code,
      lists, tables, etc. are styled per [theme.markdown].
      Markdown state (fenced-code-open) threads across lines;
      priming walks doc rows [0..top_line) so a viewport that
      opens inside a code block picks up the right style.
    - Content not covered by spans gets padded to full width
      with spaces so prior frame output doesn't leak.

    Tests capture output via [Terminal.with_capture]. *)
