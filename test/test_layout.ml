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
			render_mode = Markdown;
			min_rows = 20;
			content_inset_top = 0;
			content_inset_bottom = 0;
			content_inset_left = 0;
			content_inset_right = 0;
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
			render_mode = Markdown;
			min_rows = 20;
			content_inset_top = 0;
			content_inset_bottom = 0;
			content_inset_left = 0;
			content_inset_right = 0;
		} in
		let t' = Layout.replace_leaf t ~path:[ 42 ] dummy in
		(* original tree preserved *)
		assert (count_leaves t' = 1));

	(* ------- focus_move ------- *)

	let canvas = Rect.make ~row:0 ~col:0 ~rows:24 ~cols:80 in

	test "focus_move left from right pane of V-split" (fun () ->
		let t = Layout.single Buf.empty () in
		let t1 = Layout.split t Vertical Buf.empty () in
		(* focus starts on right (new) pane; move left *)
		let t2 = Layout.focus_move t1 ~rect:canvas `Left in
		assert (t2.focus_path = [ 0 ]));

	test "focus_move left from left pane is no-op" (fun () ->
		let t = Layout.single Buf.empty () in
		let t1 = Layout.split t Vertical Buf.empty () in
		let t1_left = { t1 with focus_path = [ 0 ] } in
		let t2 = Layout.focus_move t1_left ~rect:canvas `Left in
		assert (t2.focus_path = [ 0 ]));

	test "focus_move right from right pane is no-op" (fun () ->
		let t = Layout.single Buf.empty () in
		let t1 = Layout.split t Vertical Buf.empty () in
		let t2 = Layout.focus_move t1 ~rect:canvas `Right in
		assert (t2.focus_path = [ 1 ]));

	test "focus_move up on V-split is no-op (no vertical neighbor)" (fun () ->
		let t = Layout.single Buf.empty () in
		let t1 = Layout.split t Vertical Buf.empty () in
		let t2 = Layout.focus_move t1 ~rect:canvas `Up in
		assert (t2.focus_path = t1.focus_path));

	test "focus_move down on H-split moves to bottom" (fun () ->
		let t = Layout.single Buf.empty () in
		let t1 = Layout.split t Horizontal Buf.empty () in
		(* new leaf at [1] is the BOTTOM half. Move focus back up first. *)
		let t1_top = { t1 with focus_path = [ 0 ] } in
		let t2 = Layout.focus_move t1_top ~rect:canvas `Down in
		assert (t2.focus_path = [ 1 ]));

	test "focus_move across nested H-and-V splits" (fun () ->
		(* Construct: V split at root, right side is H split.
		   Tree: [0] = left leaf (full height),
		         [1;0] = top-right leaf, [1;1] = bottom-right leaf *)
		let t = Layout.single Buf.empty () in
		let t1 = Layout.split t Vertical Buf.empty () in
		let t2 = Layout.split t1 Horizontal Buf.empty () in
		(* focus at [1;1] — bottom-right *)
		assert (t2.focus_path = [ 1; 1 ]);
		(* Move left: should go to the left pane [0] *)
		let t3 = Layout.focus_move t2 ~rect:canvas `Left in
		assert (t3.focus_path = [ 0 ]));

	(* ------- resize ------- *)

	let split_ratio t =
		match t.Layout.root with
		| Split s -> s.ratio
		| _ -> -1.0
	in

	(* 100 cols on canvas; 20-col minimum each side gives ratio
	   bounds of [0.20, 0.80]. *)
	let canvas100 = Rect.make ~row:0 ~col:0 ~rows:24 ~cols:100 in

	test "resize grows right (focused) pane by shrinking ratio" (fun () ->
		let t = Layout.single Buf.empty () in
		let t1 = Layout.split t Vertical Buf.empty () in
		(* focus at [1] (right pane). delta +0.05 → right grows → ratio -0.05 *)
		let t2 = Layout.resize t1 ~rect:canvas100 ~delta:0.05 in
		let r = split_ratio t2 in
		assert (abs_float (r -. 0.45) < 1e-9));

	test "resize grows left (focused) pane by increasing ratio" (fun () ->
		let t = Layout.single Buf.empty () in
		let t1 = Layout.split t Vertical Buf.empty () in
		let t1_left = { t1 with focus_path = [ 0 ] } in
		let t2 = Layout.resize t1_left ~rect:canvas100 ~delta:0.05 in
		let r = split_ratio t2 in
		assert (abs_float (r -. 0.55) < 1e-9));

	test "resize clamps at 20-col minimum (lower)" (fun () ->
		let t = Layout.single Buf.empty () in
		let t1 = Layout.split t Vertical Buf.empty () in
		let t1_left = { t1 with focus_path = [ 0 ] } in
		(* delta -1.0 pushes ratio negative; clamp to 20/100=0.2 *)
		let t2 = Layout.resize t1_left ~rect:canvas100 ~delta:(-1.0) in
		let r = split_ratio t2 in
		assert (abs_float (r -. 0.20) < 1e-9));

	test "resize clamps at 20-col minimum (upper)" (fun () ->
		let t = Layout.single Buf.empty () in
		let t1 = Layout.split t Vertical Buf.empty () in
		let t1_left = { t1 with focus_path = [ 0 ] } in
		let t2 = Layout.resize t1_left ~rect:canvas100 ~delta:1.0 in
		let r = split_ratio t2 in
		(* Upper clamp: 1 - 20/100 = 0.80 *)
		assert (abs_float (r -. 0.80) < 1e-9));

	test "resize falls back to [0.05, 0.95] when canvas too small" (fun () ->
		(* 30 cols < 2 * 20 = too small for 20-col min both sides.
		   ratio_bounds degrades to the ratio floor. *)
		let small = Rect.make ~row:0 ~col:0 ~rows:24 ~cols:30 in
		let t = Layout.single Buf.empty () in
		let t1 = Layout.split t Vertical Buf.empty () in
		let t1_left = { t1 with focus_path = [ 0 ] } in
		let t2 = Layout.resize t1_left ~rect:small ~delta:(-1.0) in
		let r = split_ratio t2 in
		assert (abs_float (r -. 0.05) < 1e-9));

	test "resize on root leaf is a no-op" (fun () ->
		let t = Layout.single Buf.empty () in
		let t' = Layout.resize t ~rect:canvas100 ~delta:0.05 in
		assert (count_leaves t' = 1);
		assert (t'.focus_path = []));

	(* ------- equalize ------- *)

	test "equalize single leaf is no-op" (fun () ->
		let t = Layout.single Buf.empty () in
		let t' = Layout.equalize t in
		assert (count_leaves t' = 1));

	test "equalize sets root split ratio to 0.5" (fun () ->
		let t = Layout.single Buf.empty () in
		let t1 = Layout.split t Vertical Buf.empty () in
		let t_skewed = Layout.resize
			{ t1 with focus_path = [ 0 ] }
			~rect:canvas100 ~delta:0.3
		in
		assert (split_ratio t_skewed <> 0.5);
		let t_eq = Layout.equalize t_skewed in
		assert (split_ratio t_eq = 0.5));

	test "equalize is recursive" (fun () ->
		let t = Layout.single Buf.empty () in
		let t1 = Layout.split t Horizontal Buf.empty () in
		let t2 = Layout.split t1 Vertical Buf.empty () in
		let t_skewed1 = Layout.resize
			{ t2 with focus_path = [ 0 ] }
			~rect:canvas100 ~delta:0.2
		in
		let t_skewed2 = Layout.resize
			{ t_skewed1 with focus_path = [ 1; 0 ] }
			~rect:canvas100 ~delta:0.1
		in
		let t_eq = Layout.equalize t_skewed2 in
		(* Root is H-split; its left child is a Leaf; right child is V-split. *)
		match t_eq.root with
		| Split { ratio = r1; right = Split { ratio = r2; _ }; _ } ->
				assert (r1 = 0.5);
				assert (r2 = 0.5)
		| _ -> failwith "unexpected tree shape");

	test "equalize preserves focus" (fun () ->
		let t = Layout.single Buf.empty () in
		let t1 = Layout.split t Vertical Buf.empty () in
		let t' = Layout.equalize t1 in
		assert (t'.focus_path = t1.focus_path));

	(* ------- render ------- *)

	let contains haystack needle =
		let hl = String.length haystack in
		let nl = String.length needle in
		if nl > hl then false
		else
			let rec loop i =
				if i + nl > hl then false
				else if String.sub haystack i nl = needle then true
				else loop (i + 1)
			in
			loop 0
	in

	test "render single leaf no title emits only content" (fun () ->
		let t = Layout.single Buf.empty () in
		let r = Rect.make ~row:0 ~col:0 ~rows:3 ~cols:10 in
		let out, _cursor = Terminal.with_capture (fun () ->
			Layout.render t ~rect:r ~theme:Theme.default) in
		(* No title → no style-ANSI codes for chrome *)
		assert (not (contains out "title"));
		(* Expect 3 CSI moves (one per content row) *)
		let csi_count = ref 0 in
		String.iter (fun c ->
			if c = '\x1b' then incr csi_count
		) out;
		assert (!csi_count = 3));

	test "render leaf with title includes title text" (fun () ->
		let t = Layout.single ~title:"hello" Buf.empty () in
		let r = Rect.make ~row:0 ~col:0 ~rows:3 ~cols:20 in
		let out, _cursor = Terminal.with_capture (fun () ->
			Layout.render t ~rect:r ~theme:Theme.default) in
		assert (contains out "hello"));

	test "render truncates long title to width" (fun () ->
		let t = Layout.single
			~title:"this is a rather long title"
			Buf.empty ()
		in
		let r = Rect.make ~row:0 ~col:0 ~rows:3 ~cols:10 in
		let out, _cursor = Terminal.with_capture (fun () ->
			Layout.render t ~rect:r ~theme:Theme.default) in
		(* Title truncated to 10 bytes, so "this is a" prefix still there *)
		assert (contains out "this is a "));

	test "render pads short title to width" (fun () ->
		let t = Layout.single ~title:"ab" Buf.empty () in
		let r = Rect.make ~row:0 ~col:0 ~rows:2 ~cols:10 in
		let out, _cursor = Terminal.with_capture (fun () ->
			Layout.render t ~rect:r ~theme:Theme.default) in
		(* Title text is styled, trailing padding is plain so the
		   highlight surrounds just the text. Both pieces must be
		   present to reach full width = 10. *)
		assert (contains out "ab");
		assert (contains out "        "));   (* ≥ 8 spaces of padding *)

	test "render 2x2 split contains all four titles" (fun () ->
		let t = Layout.single ~title:"TL" Buf.empty () in
		let t = Layout.split t Vertical ~title:"TR" Buf.empty () in
		(* focus [1]; move focus back to root to split horizontally
		   — but split operates on the focused leaf, so focus first. *)
		let t = { t with focus_path = [ 0 ] } in
		let t = Layout.split t Horizontal ~title:"BL" Buf.empty () in
		let t = { t with focus_path = [ 1 ] } in
		let t = Layout.split t Horizontal ~title:"BR" Buf.empty () in
		let r = Rect.make ~row:0 ~col:0 ~rows:24 ~cols:80 in
		let out, _cursor = Terminal.with_capture (fun () ->
			Layout.render t ~rect:r ~theme:Theme.default) in
		assert (contains out "TL");
		assert (contains out "TR");
		assert (contains out "BL");
		assert (contains out "BR"));

	test "render focused vs unfocused title use different styles" (fun () ->
		let t = Layout.single ~title:"A" Buf.empty () in
		let t = Layout.split t Vertical ~title:"B" Buf.empty () in
		let r = Rect.make ~row:0 ~col:0 ~rows:3 ~cols:20 in
		let out, _cursor = Terminal.with_capture (fun () ->
			Layout.render t ~rect:r ~theme:Theme.default) in
		(* Focused style for default theme includes bold + bg24 *)
		let focused_ansi =
			Theme.style_to_ansi Theme.default.chrome.title_focused
		in
		let unfocused_ansi =
			Theme.style_to_ansi Theme.default.chrome.title_unfocused
		in
		assert (focused_ansi <> unfocused_ansi);
		assert (contains out focused_ansi);
		assert (contains out unfocused_ansi));

	test "render content reads buf.doc starting at top_line" (fun () ->
		let doc = Doc.of_string "line0\nline1\nline2\nline3\nline4" in
		let b = Buf.of_doc doc in
		(* Make a pane with custom viewport starting at line 2 *)
		let t = Layout.single b () in
		let t = match Layout.find_leaf t ~path:[] with
			| Some pane ->
					let pane' = {
						pane with
						viewport = { pane.viewport with top_line = 2 }
					} in
					Layout.replace_leaf t ~path:[] pane'
			| None -> t
		in
		let r = Rect.make ~row:0 ~col:0 ~rows:3 ~cols:20 in
		let out, _cursor = Terminal.with_capture (fun () ->
			Layout.render t ~rect:r ~theme:Theme.default) in
		(* top_line=2 means first visible row is line2 *)
		assert (contains out "line2"));

	test "render empty-rect leaf produces no output" (fun () ->
		let t = Layout.single ~title:"X" Buf.empty () in
		let r = Rect.make ~row:0 ~col:0 ~rows:0 ~cols:0 in
		let out, _cursor = Terminal.with_capture (fun () ->
			Layout.render t ~rect:r ~theme:Theme.default) in
		assert (out = ""));

	(* ------- markdown-styled content rendering (spec §9) ------- *)

	let render_capture t r =
		let out, _cursor = Terminal.with_capture (fun () ->
			Layout.render t ~rect:r ~theme:Theme.default) in
		out
	in

	test "render emits heading style for # line" (fun () ->
		let doc = Doc.of_string "# Hello" in
		let t = Layout.single (Buf.of_doc doc) () in
		let r = Rect.make ~row:0 ~col:0 ~rows:2 ~cols:20 in
		let out = render_capture t r in
		let heading_ansi =
			Theme.style_to_ansi Theme.default.markdown.heading.(0)
		in
		assert (heading_ansi <> "");
		assert (contains out heading_ansi));

	test "render emits bold style for **...**" (fun () ->
		let doc = Doc.of_string "this **word** here" in
		let t = Layout.single (Buf.of_doc doc) () in
		let r = Rect.make ~row:0 ~col:0 ~rows:2 ~cols:40 in
		let out = render_capture t r in
		let bold_ansi =
			Theme.style_to_ansi Theme.default.markdown.bold
		in
		assert (bold_ansi <> "");
		assert (contains out bold_ansi));

	test "render styles inline code" (fun () ->
		let doc = Doc.of_string "use `fn` please" in
		let t = Layout.single (Buf.of_doc doc) () in
		let r = Rect.make ~row:0 ~col:0 ~rows:2 ~cols:40 in
		let out = render_capture t r in
		let code_ansi =
			Theme.style_to_ansi Theme.default.markdown.inline_code
		in
		assert (code_ansi <> "");
		assert (contains out code_ansi));

	test "render carries fenced-code state across lines" (fun () ->
		(* Line in middle of fenced block should be styled as code
		   even though the line itself has no ``` marker. *)
		let doc = Doc.of_string "```\nlet x = 1\n```\nafter" in
		let t = Layout.single (Buf.of_doc doc) () in
		let r = Rect.make ~row:0 ~col:0 ~rows:4 ~cols:40 in
		let out = render_capture t r in
		let code_block_ansi =
			Theme.style_to_ansi Theme.default.markdown.code_block
		in
		assert (code_block_ansi <> "");
		(* The middle line should be emitted inside code_block style *)
		assert (contains out code_block_ansi));

	test "render primes fenced state from doc lines above viewport" (fun () ->
		(* Viewport starts at line 2 (inside a fence).  Without the
		   priming loop, tokenizer would treat "let x = 1" as a plain
		   markdown line. With priming, it gets code_block style. *)
		let doc = Doc.of_string "```ocaml\nlet a = 1\nlet b = 2\nlet c = 3" in
		let t = Layout.single (Buf.of_doc doc) () in
		let t =
			match Layout.find_leaf t ~path:[] with
			| Some pane ->
					let p = {
						pane with
						viewport = { pane.viewport with top_line = 2 }
					} in
					Layout.replace_leaf t ~path:[] p
			| None -> t
		in
		let r = Rect.make ~row:0 ~col:0 ~rows:2 ~cols:40 in
		let out = render_capture t r in
		let code_block_ansi =
			Theme.style_to_ansi Theme.default.markdown.code_block
		in
		assert (contains out code_block_ansi));

	test "render styles table pipes" (fun () ->
		let doc = Doc.of_string "| a | b |" in
		let t = Layout.single (Buf.of_doc doc) () in
		let r = Rect.make ~row:0 ~col:0 ~rows:2 ~cols:20 in
		let out = render_capture t r in
		let border_ansi =
			Theme.style_to_ansi Theme.default.markdown.table_border
		in
		assert (border_ansi <> "");
		assert (contains out border_ansi));

	test "render pads short line to full width with spaces" (fun () ->
		let doc = Doc.of_string "hi" in
		let t = Layout.single (Buf.of_doc doc) () in
		let r = Rect.make ~row:0 ~col:0 ~rows:1 ~cols:10 in
		let out = render_capture t r in
		(* "hi" + 8 spaces should appear somewhere in the output *)
		assert (contains out "hi        "));

	print_endline "test_layout done"
