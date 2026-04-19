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
