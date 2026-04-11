open Twig

let test name f =
	try
		f ();
		Printf.printf "  ok  %s\n" name
	with e ->
		Printf.printf "fail  %s: %s\n" name (Printexc.to_string e);
		exit 1

let () =
	test "empty doc has 0 lines" (fun () ->
		assert (Doc.line_count Doc.empty = 0));

	test "of_string empty" (fun () ->
		assert (Doc.line_count (Doc.of_string "") = 0));

	test "of_string single line" (fun () ->
		let d = Doc.of_string "hello" in
		assert (Doc.line_count d = 1);
		assert (Doc.get_line 0 d = Some "hello"));

	test "of_string multiple lines" (fun () ->
		let d = Doc.of_string "a\nb\nc" in
		assert (Doc.line_count d = 3);
		assert (Doc.get_line 0 d = Some "a");
		assert (Doc.get_line 1 d = Some "b");
		assert (Doc.get_line 2 d = Some "c"));

	test "to_string roundtrips of_string" (fun () ->
		let s = "alpha\nbeta\ngamma" in
		assert (Doc.to_string (Doc.of_string s) = s));

	test "get_line out of bounds returns None" (fun () ->
		let d = Doc.of_string "hello\nworld" in
		assert (Doc.get_line (-1) d = None);
		assert (Doc.get_line 2 d = None));

	test "measure counts lines and bytes" (fun () ->
		let d = Doc.of_string "abc\nde\nf" in
		let m = Doc.measure d in
		assert (m.lines = 3);
		assert (m.bytes = 8));

	test "measure of empty" (fun () ->
		let m = Doc.measure Doc.empty in
		assert (m.lines = 0);
		assert (m.bytes = 0));

	test "insert_line at start" (fun () ->
		let d = Doc.of_string "a\nb" in
		let d = Doc.insert_line 0 "first" d in
		assert (Doc.line_count d = 3);
		assert (Doc.get_line 0 d = Some "first");
		assert (Doc.get_line 1 d = Some "a");
		assert (Doc.get_line 2 d = Some "b"));

	test "insert_line in middle" (fun () ->
		let d = Doc.of_string "a\nc" in
		let d = Doc.insert_line 1 "b" d in
		assert (Doc.line_count d = 3);
		assert (Doc.get_line 0 d = Some "a");
		assert (Doc.get_line 1 d = Some "b");
		assert (Doc.get_line 2 d = Some "c"));

	test "insert_line at end" (fun () ->
		let d = Doc.of_string "a\nb" in
		let d = Doc.insert_line 2 "c" d in
		assert (Doc.line_count d = 3);
		assert (Doc.get_line 2 d = Some "c"));

	test "insert_line into empty" (fun () ->
		let d = Doc.insert_line 0 "only" Doc.empty in
		assert (Doc.line_count d = 1);
		assert (Doc.get_line 0 d = Some "only"));

	test "delete_line middle" (fun () ->
		let d = Doc.of_string "a\nb\nc" in
		let d = Doc.delete_line 1 d in
		assert (Doc.line_count d = 2);
		assert (Doc.get_line 0 d = Some "a");
		assert (Doc.get_line 1 d = Some "c"));

	test "delete_line first" (fun () ->
		let d = Doc.of_string "a\nb\nc" in
		let d = Doc.delete_line 0 d in
		assert (Doc.line_count d = 2);
		assert (Doc.get_line 0 d = Some "b"));

	test "delete_line out of bounds is a no-op" (fun () ->
		let d = Doc.of_string "a\nb" in
		let d' = Doc.delete_line 10 d in
		assert (Doc.line_count d' = 2));

	test "replace_line" (fun () ->
		let d = Doc.of_string "a\nb\nc" in
		let d = Doc.replace_line 1 "B" d in
		assert (Doc.get_line 1 d = Some "B");
		assert (Doc.get_line 0 d = Some "a");
		assert (Doc.get_line 2 d = Some "c"));

	test "old version unchanged after insert" (fun () ->
		let d1 = Doc.of_string "a\nb" in
		let _d2 = Doc.insert_line 1 "X" d1 in
		assert (Doc.line_count d1 = 2);
		assert (Doc.get_line 1 d1 = Some "b"));

	test "old version unchanged after delete" (fun () ->
		let d1 = Doc.of_string "a\nb\nc" in
		let _d2 = Doc.delete_line 1 d1 in
		assert (Doc.line_count d1 = 3);
		assert (Doc.get_line 1 d1 = Some "b"));

	print_endline "all tests passed"
