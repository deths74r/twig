type dir = Horizontal | Vertical

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

let make_pane ?title ?(render_mode=Markdown) ?(min_rows=0) buf = {
	buf;
	viewport = default_viewport;
	title;
	render_mode;
	min_rows;
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

let single ?title ?render_mode ?min_rows buf () =
	{ root = Leaf (make_pane ?title ?render_mode ?min_rows buf); focus_path = [] }

let split t dir ?title ?render_mode ?min_rows buf () =
	match find_subtree_opt t.root t.focus_path with
	| None -> t
	| Some original ->
			let new_leaf = Leaf (make_pane ?title ?render_mode ?min_rows buf) in
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
(* Swap                                                               *)
(* ------------------------------------------------------------------ *)

let swap t ~rect dir =
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
			let (target_path, target_pane, _) = best in
			(* Get the focused pane *)
			(match find_leaf t ~path:t.focus_path with
			 | None -> t
			 | Some focused_pane ->
				 (* Swap: put focused at target's path, target at focused's path *)
				 let t = replace_leaf t ~path:target_path focused_pane in
				 let t = replace_leaf t ~path:t.focus_path target_pane in
				 (* Focus follows the moved pane *)
				 { t with focus_path = target_path })

(* ------------------------------------------------------------------ *)
(* Fullscreen                                                         *)
(* ------------------------------------------------------------------ *)

type fullscreen_stash = {
	stashed_root : tree;
	stashed_focus : path;
}

let fullscreen_stash : fullscreen_stash option ref = ref None

let fullscreen t =
	match !fullscreen_stash with
	| Some stash ->
		(* Exit fullscreen: restore stashed tree, update the
		   focused pane's buf with whatever changed while
		   fullscreened *)
		let updated_pane = focus t in
		let restored = { root = stash.stashed_root;
						 focus_path = stash.stashed_focus } in
		let restored = match updated_pane with
			| Some pane -> replace_leaf restored
				~path:stash.stashed_focus pane
			| None -> restored
		in
		fullscreen_stash := None;
		restored
	| None ->
		(* Enter fullscreen: stash tree, replace root with focused leaf *)
		match focus t with
		| None -> t
		| Some pane ->
			fullscreen_stash := Some {
				stashed_root = t.root;
				stashed_focus = t.focus_path;
			};
			{ root = Leaf pane; focus_path = [] }

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
    with [dir] inside [parent_rect] get at least their minimum size. *)
let ratio_bounds (dir : dir) (parent_rect : Rect.t) (s : tree) =
	let rec min_req tree =
		match tree with
		| Leaf p ->
				(match dir with
				 | Horizontal -> max p.min_rows minimum_cells
				 | Vertical -> minimum_cells)
		| Split s_child ->
				if s_child.dir = dir then
					min_req s_child.left + min_req s_child.right
				else
					max (min_req s_child.left) (min_req s_child.right)
	in
	let extent =
		match dir with
		| Horizontal -> parent_rect.rows
		| Vertical -> parent_rect.cols
	in
	match s with
	| Leaf _ -> (0.05, 0.95)
	| Split { left; right; _ } ->
		let min_left = min_req left in
		let min_right = min_req right in
		
		if extent <= min_left + min_right then
			(* Not enough space. Fall back to the old ratio floor so we don't
			   produce unusable 0/everything splits. *)
			(0.05, 0.95)
		else
			let lo = float_of_int min_left /. float_of_int extent in
			let hi = 1.0 -. (float_of_int min_right /. float_of_int extent) in
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
					let lo, hi = ratio_bounds s.dir parent_rect (Split s) in
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

let render_content (pane : pane) (rect : Rect.t) ~theme ~focused =
	let title_rows = if pane.title <> None then 1 else 0 in
	let content_start = rect.row + title_rows in
	let content_rows = rect.rows - title_rows in
	let cursor_pos = ref None in
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
		   may produce multiple screen rows via wrap_line.

		   Plain-mode panes ALWAYS render doc_row 0 (with the line
		   defaulting to "") even when [total_lines = 0]: a Plain
		   pane has a prefix that should always show — that's the
		   whole point of the render mode. Markdown panes skip the
		   empty-doc case (nothing to tokenize). *)
		let effective_total =
			match pane.render_mode with
			| Markdown -> total_lines
			| Plain _ -> max 1 total_lines
		in
		let screen_row = ref 0 in
		let doc_row = ref top in
		while !screen_row < content_rows && !doc_row < effective_total do
			let line =
				Option.value (Doc.get_line !doc_row doc) ~default:""
			in
			
			let segments, prefix_len, line_text, spans =
				match pane.render_mode with
				| Markdown ->
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
						segments, 0, line, spans
				| Plain { prefix_first; prefix_rest; prefix_style } ->
						let prefix = if !doc_row = 0 then prefix_first else prefix_rest in
						let prefix_byte_len = String.length prefix in
						let prefix_len = Grapheme.count prefix in
						let line_text = prefix ^ line in
						let line_text_len = String.length line_text in
						let segments =
							if pane.viewport.Viewport.wrap then
								Viewport.wrap_line line_text wrap_width
							else
								[{ Viewport.start_gi = 0;
								   end_gi = Grapheme.count line_text }]
						in
						(* Spans must cover every byte of line_text per
						   Markdown.tokenize_line's contract (render_line
						   drops any byte not in a span). Prefix gets the
						   optional style, the rest gets a plain span so
						   typed content actually renders. *)
						let prefix_span =
							match prefix_style with
							| Some style ->
									[{ Markdown.start = 0;
									   stop = prefix_byte_len;
									   style }]
							| None ->
									[{ Markdown.start = 0;
									   stop = prefix_byte_len;
									   style = Theme.plain }]
						in
						let rest_span =
							if prefix_byte_len < line_text_len then
								[{ Markdown.start = prefix_byte_len;
								   stop = line_text_len;
								   style = Theme.plain }]
							else []
						in
						segments, prefix_len, line_text, prefix_span @ rest_span
			in
			
			let selection_spans =
				match pane.buf.mark with
				| None -> []
				| Some mark ->
					let start_pos, end_pos =
						if Position.compare mark pane.buf.cursor <= 0 then mark, pane.buf.cursor
						else pane.buf.cursor, mark
					in
					if !doc_row >= start_pos.line && !doc_row <= end_pos.line then begin
						let start_byte =
							if !doc_row = start_pos.line then
								Grapheme.byte_of_index line_text (start_pos.column + prefix_len)
							else 0
						in
						let end_byte =
							if !doc_row = end_pos.line then
								Grapheme.byte_of_index line_text (end_pos.column + prefix_len)
							else String.length line_text
						in
						if start_byte < end_byte then
							[{ Markdown.start = start_byte; stop = end_byte; style = theme.Theme.syntax.selection }]
						else []
					end else []
			in
			let spans = spans @ selection_spans in
			
			List.iter
				(fun (seg : Viewport.segment) ->
					if !screen_row < content_rows then begin
						(* Extract the substring for this segment *)
						let seg_start_byte =
							Grapheme.byte_of_index line_text seg.start_gi
						in
						let seg_end_byte =
							Grapheme.byte_of_index line_text seg.end_gi
						in
						let seg_text = String.sub line_text seg_start_byte
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
						
						(* Track cursor position *)
						if focused && !doc_row = pane.buf.cursor.line then begin
							let cursor_col = pane.buf.cursor.column + prefix_len in
							if cursor_col >= seg.start_gi && cursor_col <= seg.end_gi then begin
								(* Only set cursor if it falls in the *visible* part of the line.
								   For wrapped lines, the cursor is on the segment where it's <= end_gi,
								   except if it's strictly < end_gi OR it's the very end of the line. *)
								let is_last_seg = seg.end_gi = Grapheme.count line_text in
								if cursor_col < seg.end_gi || is_last_seg then
									cursor_pos := Some (content_start + !screen_row, rect.col + (cursor_col - seg.start_gi))
							end
						end;
						
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
	end;
	!cursor_pos

let render t ~rect ~theme =
	let leaves = leaf_rects_of_tree t.root rect in
	let cursor_pos = ref None in
	List.iter
		(fun (path, pane, r) ->
			if not (Rect.is_empty r) then
				Terminal.with_clip r (fun () ->
					let focused = path = t.focus_path in
					render_title pane r ~focused ~theme;
					let c = render_content pane r ~theme ~focused in
					if focused then cursor_pos := c))
		leaves;
	!cursor_pos
