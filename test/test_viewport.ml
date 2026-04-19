open Twig

let test name f =
	try
		f ();
		Printf.printf "  ok  %s\n" name
	with e ->
		Printf.printf "fail  %s: %s\n" name (Printexc.to_string e);
		exit 1

let () =
	(* ------- make ------- *)

	test "make sets rows and cols" (fun () ->
		let vp = Viewport.make ~rows:24 ~cols:80 in
		assert (vp.rows = 24);
		assert (vp.cols = 80));

	test "make starts at top_line 0" (fun () ->
		let vp = Viewport.make ~rows:10 ~cols:20 in
		assert (vp.top_line = 0));

	test "make enables wrap by default" (fun () ->
		let vp = Viewport.make ~rows:10 ~cols:20 in
		assert (vp.wrap = true));

	test "make clamps negative rows to 0" (fun () ->
		let vp = Viewport.make ~rows:(-5) ~cols:20 in
		assert (vp.rows = 0));

	test "make clamps negative cols to 0" (fun () ->
		let vp = Viewport.make ~rows:10 ~cols:(-5) in
		assert (vp.cols = 0));

	(* ------- scroll_to ------- *)

	test "scroll_to moves top_line" (fun () ->
		let vp = Viewport.make ~rows:10 ~cols:20 in
		let vp' = Viewport.scroll_to vp ~line:42 in
		assert (vp'.top_line = 42));

	test "scroll_to preserves other fields" (fun () ->
		let vp = Viewport.make ~rows:10 ~cols:20 in
		let vp' = Viewport.scroll_to vp ~line:5 in
		assert (vp'.rows = 10);
		assert (vp'.cols = 20);
		assert (vp'.wrap = true));

	test "scroll_to clamps negative line to 0" (fun () ->
		let vp = Viewport.make ~rows:10 ~cols:20 in
		let vp' = Viewport.scroll_to vp ~line:(-3) in
		assert (vp'.top_line = 0));

	test "scroll_to is idempotent for same line" (fun () ->
		let vp = Viewport.make ~rows:10 ~cols:20 in
		let vp' = Viewport.scroll_to vp ~line:5 in
		let vp'' = Viewport.scroll_to vp' ~line:5 in
		assert (vp'.top_line = vp''.top_line));

	(* ------- wrap math retained ------- *)

	test "wrap_line short line is one segment" (fun () ->
		let segs = Viewport.wrap_line "hello" 80 in
		assert (List.length segs = 1));

	test "wrap_line empty is one empty segment" (fun () ->
		let segs = Viewport.wrap_line "" 10 in
		assert (List.length segs = 1);
		match segs with
		| [ s ] -> assert (s.start_gi = 0 && s.end_gi = 0)
		| _ -> failwith "expected one segment");

	test "wrap_line long line splits" (fun () ->
		let s = String.make 25 'x' in
		let segs = Viewport.wrap_line s 10 in
		assert (List.length segs >= 2));

	print_endline "test_viewport done"
