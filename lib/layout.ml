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
let leaf_rects tree root_rect =
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

(* ------------------------------------------------------------------ *)
(* Focus                                                              *)
(* ------------------------------------------------------------------ *)

let focus_move t ~rect dir =
	let leaves = leaf_rects t.root rect in
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

let clamp_ratio r =
	if r < 0.05 then 0.05
	else if r > 0.95 then 0.95
	else r

let resize t ~delta =
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
					let new_ratio = clamp_ratio (s.ratio +. (delta *. sign)) in
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
