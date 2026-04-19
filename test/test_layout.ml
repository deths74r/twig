open Twig

let test name f =
	try
		f ();
		Printf.printf "  ok  %s\n" name
	with e ->
		Printf.printf "fail  %s: %s\n" name (Printexc.to_string e);
		exit 1

let count_leaves t =
	let n = ref 0 in
	Layout.iter_leaves t ~f:(fun _ _ -> incr n);
	!n

let paths_of t =
	let ps = ref [] in
	Layout.iter_leaves t ~f:(fun p _ -> ps := p :: !ps);
	List.rev !ps

let () =
	(* ------- single ------- *)

	test "single creates one-leaf tree" (fun () ->
		let t = Layout.single Buf.empty () in
		assert (count_leaves t = 1);
		assert (t.focus_path = []));

	test "single with title" (fun () ->
		let t = Layout.single ~title:"convo" Buf.empty () in
		match Layout.focus t with
		| Some pane -> assert (pane.title = Some "convo")
		| None -> failwith "expected focused pane");

	test "single pane focus is accessible" (fun () ->
		let t = Layout.single Buf.empty () in
		match Layout.focus t with
		| Some _ -> ()
		| None -> failwith "focus None on single pane");

	(* ------- split ------- *)

	test "split horizontal creates Split tree" (fun () ->
		let t = Layout.single Buf.empty () in
		let t' = Layout.split t Horizontal Buf.empty () in
		assert (count_leaves t' = 2);
		match t'.root with
		| Split { dir = Horizontal; _ } -> ()
		| _ -> failwith "expected horizontal Split at root");

	test "split vertical creates Split tree" (fun () ->
		let t = Layout.single Buf.empty () in
		let t' = Layout.split t Vertical Buf.empty () in
		match t'.root with
		| Split { dir = Vertical; _ } -> ()
		| _ -> failwith "expected vertical Split at root");

	test "split focus moves to new pane" (fun () ->
		let t = Layout.single Buf.empty () in
		let t' = Layout.split t Horizontal Buf.empty () in
		assert (t'.focus_path = [ 1 ]));

	test "split preserves original leaf" (fun () ->
		let b1 = Buf.of_doc (Doc.of_string "one") in
		let b2 = Buf.of_doc (Doc.of_string "two") in
		let t = Layout.single ~title:"a" b1 () in
		let t' = Layout.split t Horizontal ~title:"b" b2 () in
		match Layout.find_leaf t' ~path:[ 0 ] with
		| Some p -> assert (p.title = Some "a")
		| None -> failwith "missing original pane");

	test "split starts new pane at default ratio 0.5" (fun () ->
		let t = Layout.single Buf.empty () in
		let t' = Layout.split t Horizontal Buf.empty () in
		match t'.root with
		| Split { ratio; _ } ->
				assert (ratio = 0.5)
		| _ -> failwith "expected Split");

	test "nested split goes into currently-focused leaf" (fun () ->
		let t = Layout.single Buf.empty () in
		let t1 = Layout.split t Horizontal Buf.empty () in
		(* focus is at [1], splitting again should split THAT leaf *)
		let t2 = Layout.split t1 Vertical Buf.empty () in
		assert (count_leaves t2 = 3);
		(* focus now at [1; 1] *)
		assert (t2.focus_path = [ 1; 1 ]));

	test "iter_leaves on nested tree yields correct paths" (fun () ->
		let t = Layout.single Buf.empty () in
		let t1 = Layout.split t Horizontal Buf.empty () in
		let t2 = Layout.split t1 Vertical Buf.empty () in
		let ps = paths_of t2 in
		assert (ps = [ [ 0 ]; [ 1; 0 ]; [ 1; 1 ] ]));

	(* ------- close ------- *)

	test "close on root-only returns None" (fun () ->
		let t = Layout.single Buf.empty () in
		assert (Layout.close t = None));

	test "close one-level split collapses to sibling" (fun () ->
		let t = Layout.single Buf.empty () in
		let t1 = Layout.split t Horizontal Buf.empty () in
		(* focus is [1]; closing it should leave only the left leaf *)
		match Layout.close t1 with
		| None -> failwith "expected close to succeed"
		| Some t' ->
				assert (count_leaves t' = 1);
				assert (t'.focus_path = []));

	test "close left pane leaves the right" (fun () ->
		let b1 = Buf.of_doc (Doc.of_string "one") in
		let b2 = Buf.of_doc (Doc.of_string "two") in
		let t = Layout.single ~title:"a" b1 () in
		let t1 = Layout.split t Horizontal ~title:"b" b2 () in
		(* force-focus the left pane *)
		let t1_left = { t1 with focus_path = [ 0 ] } in
		match Layout.close t1_left with
		| None -> failwith "expected close"
		| Some t' ->
				match Layout.focus t' with
				| Some p -> assert (p.title = Some "b")
				| None -> failwith "expected surviving pane");

	test "close nested leaves collapse correctly" (fun () ->
		let t = Layout.single Buf.empty () in
		let t1 = Layout.split t Horizontal Buf.empty () in
		let t2 = Layout.split t1 Vertical Buf.empty () in
		(* focus at [1; 1], three leaves. close should collapse *)
		match Layout.close t2 with
		| None -> failwith "expected close"
		| Some t' ->
				assert (count_leaves t' = 2);
				(* focus should move to [1] (the parent path) *)
				assert (t'.focus_path = [ 1 ]));

	test "close preserves tree structure for non-focused subtree" (fun () ->
		let t = Layout.single Buf.empty () in
		let t1 = Layout.split t Horizontal Buf.empty () in
		let t2 = Layout.split t1 Vertical Buf.empty () in
		(* t2 has root Split { dir=H; left=Leaf; right=Split { dir=V; ... } } *)
		match Layout.close t2 with
		| None -> failwith "expected close"
		| Some t' ->
				(* After closing [1;1], parent collapses to [1;0] which is a Leaf. *)
				match t'.root with
				| Split { dir = Horizontal; left = Leaf _; right = Leaf _; _ } -> ()
				| _ -> failwith "expected H split with two leaf children");

	(* ------- find_leaf ------- *)

	test "find_leaf at root path returns focused pane" (fun () ->
		let t = Layout.single ~title:"x" Buf.empty () in
		match Layout.find_leaf t ~path:[] with
		| Some p -> assert (p.title = Some "x")
		| None -> failwith "expected pane at []");

	test "find_leaf at invalid path returns None" (fun () ->
		let t = Layout.single Buf.empty () in
		assert (Layout.find_leaf t ~path:[ 0 ] = None));

	test "find_leaf into a Split path returns None" (fun () ->
		let t = Layout.single Buf.empty () in
		let t' = Layout.split t Horizontal Buf.empty () in
		(* [] addresses the Split node itself, not a leaf *)
		assert (Layout.find_leaf t' ~path:[] = None));

	(* ------- replace_leaf ------- *)

	test "replace_leaf swaps pane at path" (fun () ->
		let b_old = Buf.of_doc (Doc.of_string "old") in
		let b_new = Buf.of_doc (Doc.of_string "new") in
		let t = Layout.single ~title:"orig" b_old () in
		let new_pane = {
			Layout.buf = b_new;
			viewport = Viewport.make ~rows:0 ~cols:0;
			title = Some "swapped";
		} in
		let t' = Layout.replace_leaf t ~path:[] new_pane in
		match Layout.focus t' with
		| Some p -> assert (p.title = Some "swapped")
		| None -> failwith "expected focus");

	test "replace_leaf on invalid path is a no-op" (fun () ->
		let t = Layout.single Buf.empty () in
		let dummy = {
			Layout.buf = Buf.empty;
			viewport = Viewport.make ~rows:0 ~cols:0;
			title = None;
		} in
		let t' = Layout.replace_leaf t ~path:[ 42 ] dummy in
		(* original tree preserved *)
		assert (count_leaves t' = 1));

	print_endline "test_layout done"
