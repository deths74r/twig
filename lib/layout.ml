type dir = Horizontal | Vertical

type pane = {
	buf      : Buf.t;
	viewport : Viewport.t;
	title    : string option;
}

type tree =
	| Leaf of pane
	| Split of {
		dir   : dir;
		ratio : float;
		left  : tree;
		right : tree;
	}

type path = int list

type t = {
	root       : tree;
	focus_path : path;
}

let default_viewport = Viewport.make ~rows:0 ~cols:0

let make_pane ?title buf = {
	buf;
	viewport = default_viewport;
	title;
}

(* ------------------------------------------------------------------ *)
(* Path helpers                                                       *)
(* ------------------------------------------------------------------ *)

let rec find_subtree_opt tree path =
	match tree, path with
	| _, [] -> Some tree
	| Split s, 0 :: rest -> find_subtree_opt s.left rest
	| Split s, 1 :: rest -> find_subtree_opt s.right rest
	| _ -> None

(** Replace the subtree at [path] with [replacement]. A path that
    does not address an existing subtree leaves the tree unchanged. *)
let rec replace_subtree tree path replacement =
	match tree, path with
	| _, [] -> replacement
	| Split s, 0 :: rest ->
			Split { s with left = replace_subtree s.left rest replacement }
	| Split s, 1 :: rest ->
			Split { s with right = replace_subtree s.right rest replacement }
	| _ -> tree

(* ------------------------------------------------------------------ *)
(* Construction                                                       *)
(* ------------------------------------------------------------------ *)

let single ?title buf () =
	{ root = Leaf (make_pane ?title buf); focus_path = [] }

let split t dir ?title buf () =
	match find_subtree_opt t.root t.focus_path with
	| None -> t
	| Some original ->
			let new_leaf = Leaf (make_pane ?title buf) in
			let replacement =
				Split { dir; ratio = 0.5;
				        left = original; right = new_leaf }
			in
			let new_root = replace_subtree t.root t.focus_path replacement in
			{ root = new_root; focus_path = t.focus_path @ [ 1 ] }

(* ------------------------------------------------------------------ *)
(* Close                                                              *)
(* ------------------------------------------------------------------ *)

let close t =
	match t.focus_path with
	| [] -> None  (* root is a leaf; nothing to close down to *)
	| _ ->
			(* The parent of the focused leaf is at [parent_path].
			   Replace that parent Split with its surviving
			   sibling subtree. Focus moves to the sibling's
			   position, which equals parent_path. *)
			let rev_path = List.rev t.focus_path in
			(match rev_path with
			 | [] -> None  (* unreachable — already handled above *)
			 | last :: rev_parent_path ->
					 let parent_path = List.rev rev_parent_path in
					 let parent =
						 match find_subtree_opt t.root parent_path with
						 | Some p -> p
						 | None -> Leaf (make_pane Buf.empty)
					 in
					 match parent with
					 | Leaf _ -> None  (* malformed — shouldn't happen *)
					 | Split s ->
							 let sibling =
								 if last = 0 then s.right else s.left
							 in
							 let new_root =
								 replace_subtree t.root parent_path sibling
							 in
							 Some { root = new_root; focus_path = parent_path })

(* ------------------------------------------------------------------ *)
(* Leaf access                                                        *)
(* ------------------------------------------------------------------ *)

let iter_leaves t ~f =
	let rec go tree rev_path =
		match tree with
		| Leaf pane -> f (List.rev rev_path) pane
		| Split s ->
				go s.left (0 :: rev_path);
				go s.right (1 :: rev_path)
	in
	go t.root []

let find_leaf t ~path =
	match find_subtree_opt t.root path with
	| Some (Leaf pane) -> Some pane
	| _ -> None

let replace_leaf t ~path pane =
	match find_subtree_opt t.root path with
	| Some (Leaf _) ->
			let new_root = replace_subtree t.root path (Leaf pane) in
			{ t with root = new_root }
	| _ -> t

let focus t = find_leaf t ~path:t.focus_path

(* ------------------------------------------------------------------ *)
(* Geometry                                                           *)
(* ------------------------------------------------------------------ *)

(** Compute the rectangle for every leaf under [tree] given a
    parent [rect]. Accumulated in right-to-left order; caller
    doesn't rely on the order. *)
let leaf_rects_of_tree tree root_rect =
	let rec go tree rect rev_path acc =
		match tree with
		| Leaf pane ->
				(List.rev rev_path, pane, rect) :: acc
		| Split s ->
				let r1, r2 =
					match s.dir with
					| Horizontal -> Rect.split_h rect ~ratio:s.ratio
					| Vertical   -> Rect.split_v rect ~ratio:s.ratio
				in
				let acc = go s.left r1 (0 :: rev_path) acc in
				go s.right r2 (1 :: rev_path) acc
	in
	go tree root_rect [] []

let leaf_rects t rect =
	leaf_rects_of_tree t.root rect

(* ------------------------------------------------------------------ *)
(* Focus                                                              *)
(* ------------------------------------------------------------------ *)

let focus_move t ~rect dir =
	let leaves = leaf_rects_of_tree t.root rect in
	let focused =
		List.find_opt (fun (p, _, _) -> p = t.focus_path) leaves
	in
	match focused with
	| None -> t
	| Some (_, _, fr) ->
			let fr_end_col = fr.Rect.col + fr.cols in
			let fr_end_row = fr.Rect.row + fr.rows in
			let fc_row = fr.row + (fr.rows / 2) in
			let fc_col = fr.col + (fr.cols / 2) in
			let in_direction (r : Rect.t) =
				match dir with
				| `Left  -> r.col + r.cols <= fr.col
				| `Right -> r.col >= fr_end_col
				| `Up    -> r.row + r.rows <= fr.row
				| `Down  -> r.row >= fr_end_row
			in
			let dist (r : Rect.t) =
				let rc_row = r.row + (r.rows / 2) in
				let rc_col = r.col + (r.cols / 2) in
				abs (rc_row - fc_row) + abs (rc_col - fc_col)
			in
			let candidates =
				List.filter
					(fun (p, _, r) -> p <> t.focus_path && in_direction r)
					leaves
			in
			match candidates with
			| [] -> t
			| first :: rest ->
					let best =
						List.fold_left
							(fun best c ->
								let (_, _, br) = best in
								let (_, _, cr) = c in
								if dist cr < dist br then c else best)
							first rest
					in
					let (p, _, _) = best in
					{ t with focus_path = p }

(* ------------------------------------------------------------------ *)
(* Resize                                                             *)
(* ------------------------------------------------------------------ *)

let minimum_cells = 20

(** Compute the rect of the subtree at [path] given [root_rect].
    Returns None if the path is invalid. *)
let rec subtree_rect tree rect path =
	match tree, path with
	| _, [] -> Some rect
	| Split s, 0 :: rest ->
			let r1, _ =
				match s.dir with
				| Horizontal -> Rect.split_h rect ~ratio:s.ratio
				| Vertical -> Rect.split_v rect ~ratio:s.ratio
			in
			subtree_rect s.left r1 rest
	| Split s, 1 :: rest ->
			let _, r2 =
				match s.dir with
				| Horizontal -> Rect.split_h rect ~ratio:s.ratio
				| Vertical -> Rect.split_v rect ~ratio:s.ratio
			in
			subtree_rect s.right r2 rest
	| _ -> None

(** Return the minimum/maximum ratio so that both sides of a split
    with [dir] inside [parent_rect] get at least [minimum_cells]. *)
let ratio_bounds (dir : dir) (parent_rect : Rect.t) =
	let extent =
		match dir with
		| Horizontal -> parent_rect.rows
		| Vertical -> parent_rect.cols
	in
	if extent <= 2 * minimum_cells then
		(* Not enough space for two minimum-sized children.
		   Fall back to the old ratio floor so we don't produce
		   unusable 0/everything splits. *)
		(0.05, 0.95)
	else
		let lo = float_of_int minimum_cells /. float_of_int extent in
		let hi = 1.0 -. lo in
		(lo, hi)

let clamp_bounded lo hi r =
	if r < lo then lo
	else if r > hi then hi
	else r

let resize t ~rect ~delta =
	match List.rev t.focus_path with
	| [] -> t  (* focus is at root; no ancestor split *)
	| last :: rev_parent ->
			let parent_path = List.rev rev_parent in
			match find_subtree_opt t.root parent_path with
			| Some (Split s) ->
					(* delta is the desired signed change to the focused
					   pane's size. If focus is the left/top child
					   (last = 0), growing it = increasing ratio.
					   If focus is the right/bottom child (last = 1),
					   growing it = decreasing ratio. *)
					let sign = if last = 0 then 1.0 else -1.0 in
					let raw = s.ratio +. (delta *. sign) in
					let parent_rect =
						match subtree_rect t.root rect parent_path with
						| Some r -> r
						| None -> rect
					in
					let lo, hi = ratio_bounds s.dir parent_rect in
					let new_ratio = clamp_bounded lo hi raw in
					let new_parent = Split { s with ratio = new_ratio } in
					let new_root =
						replace_subtree t.root parent_path new_parent
					in
					{ t with root = new_root }
			| _ -> t

(* ------------------------------------------------------------------ *)
(* Equalize                                                           *)
(* ------------------------------------------------------------------ *)

let rec equalize_tree = function
	| Leaf _ as l -> l
	| Split s ->
			Split {
				s with
				ratio = 0.5;
				left  = equalize_tree s.left;
				right = equalize_tree s.right;
			}

let equalize t = { t with root = equalize_tree t.root }

(* ------------------------------------------------------------------ *)
(* Render                                                             *)
(* ------------------------------------------------------------------ *)

(** Right-truncate [s] to fit [n] bytes. Byte-naive (the §18:9
    scope carve — ASCII chrome and pre-wrapped span content
    only; grapheme-aware width is future work). *)
let truncate_to s n =
	let len = String.length s in
	if n <= 0 then ""
	else if len <= n then s
	else String.sub s 0 n

let render_title (pane : pane) (rect : Rect.t) ~focused ~theme =
	match pane.title with
	| None -> ()
	| Some title ->
			let style =
				if focused then theme.Theme.chrome.title_focused
				else theme.chrome.title_unfocused
			in
			let text = truncate_to title rect.cols in
			let padded =
				text
				^ String.make (max 0 (rect.cols - String.length text)) ' '
			in
			Terminal.move ~row:rect.row ~col:rect.col;
			let ansi = Theme.style_to_ansi style in
			if ansi <> "" then Terminal.write ansi;
			Terminal.write padded;
			if ansi <> "" then Terminal.write Theme.reset

(** Emit one source line at [target_row] starting at [target_col],
    clipped to [max_cols] total bytes. Spans drive per-range
    styling. Remaining width past the last span is padded with
    plain spaces so prior frame content doesn't leak. *)
let render_line ~target_row ~target_col ~max_cols ~line
    ~(spans : Markdown.span list) =
	Terminal.move ~row:target_row ~col:target_col;
	let remaining = ref max_cols in
	List.iter
		(fun (span : Markdown.span) ->
			if !remaining > 0 then begin
				let span_len = span.stop - span.start in
				let emit_len =
					if span_len < !remaining then span_len else !remaining
				in
				if emit_len > 0 then begin
					let chunk = String.sub line span.start emit_len in
					let ansi = Theme.style_to_ansi span.style in
					if ansi <> "" then begin
						Terminal.write ansi;
						Terminal.write chunk;
						Terminal.write Theme.reset
					end else
						Terminal.write chunk;
					remaining := !remaining - emit_len
				end
			end)
		spans;
	if !remaining > 0 then
		Terminal.write (String.make !remaining ' ')

let render_content (pane : pane) (rect : Rect.t) ~theme =
	let title_rows = if pane.title <> None then 1 else 0 in
	let content_start = rect.row + title_rows in
	let content_rows = rect.rows - title_rows in
	if content_rows <= 0 || rect.cols <= 0 then ()
	else begin
		let doc = pane.buf.Buf.doc in
		let top = pane.viewport.Viewport.top_line in
		let total_lines = Doc.line_count doc in
		let md_theme = theme.Theme.markdown in
		let wrap_width = rect.cols in
		(* Prime Markdown state through lines above the viewport *)
		let state = ref Markdown.init in
		for doc_row = 0 to top - 1 do
			let line =
				Option.value (Doc.get_line doc_row doc) ~default:""
			in
			let _, s =
				Markdown.tokenize_line ~state:!state ~line ~theme:md_theme
			in
			state := s
		done;
		(* Render doc lines with soft wrapping. Each doc line
		   may produce multiple screen rows via wrap_line. *)
		let screen_row = ref 0 in
		let doc_row = ref top in
		while !screen_row < content_rows && !doc_row < total_lines do
			let line =
				Option.value (Doc.get_line !doc_row doc) ~default:""
			in
			let spans, s =
				Markdown.tokenize_line ~state:!state ~line ~theme:md_theme
			in
			state := s;
			let segments =
				if pane.viewport.Viewport.wrap then
					Viewport.wrap_line line wrap_width
				else
					[{ Viewport.start_gi = 0;
					   end_gi = Grapheme.count line }]
			in
			List.iter
				(fun (seg : Viewport.segment) ->
					if !screen_row < content_rows then begin
						(* Extract the substring for this segment *)
						let seg_start_byte =
							Grapheme.byte_of_index line seg.start_gi
						in
						let seg_end_byte =
							Grapheme.byte_of_index line seg.end_gi
						in
						let seg_text = String.sub line seg_start_byte
							(seg_end_byte - seg_start_byte)
						in
						(* Filter spans to this segment's byte range *)
						let seg_spans = List.filter_map
							(fun (span : Markdown.span) ->
								if span.stop <= seg_start_byte
								   || span.start >= seg_end_byte then None
								else
									let s = max span.start seg_start_byte in
									let e = min span.stop seg_end_byte in
									if s >= e then None
									else Some { span with
										start = s - seg_start_byte;
										stop = e - seg_start_byte })
							spans
						in
						render_line
							~target_row:(content_start + !screen_row)
							~target_col:rect.col
							~max_cols:rect.cols
							~line:seg_text ~spans:seg_spans;
						incr screen_row
					end)
				segments;
			incr doc_row
		done;
		(* Clear remaining screen rows *)
		for i = !screen_row to content_rows - 1 do
			Terminal.move
				~row:(content_start + i) ~col:rect.col;
			Terminal.write (String.make rect.cols ' ')
		done
	end

let render t ~rect ~theme =
	let leaves = leaf_rects_of_tree t.root rect in
	List.iter
		(fun (path, pane, r) ->
			if not (Rect.is_empty r) then
				Terminal.with_clip r (fun () ->
					let focused = path = t.focus_path in
					render_title pane r ~focused ~theme;
					render_content pane r ~theme))
		leaves
