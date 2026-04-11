open Twig

let test name f =
	try
		f ();
		Printf.printf "  ok  %s\n" name
	with e ->
		Printf.printf "fail  %s: %s\n" name (Printexc.to_string e);
		exit 1

let pos l c : Position.t = { line = l; column = c }

let () =
	test "apply Insert into empty doc" (fun () ->
		let d = Doc.empty in
		let d = Command.apply_to_doc
			(Insert { at = pos 0 0; text = "hello" })
			d
		in
		assert (Doc.line_count d = 1);
		assert (Doc.get_line 0 d = Some "hello"));

	test "apply Insert in middle of line" (fun () ->
		let d = Doc.of_string "heo" in
		let d = Command.apply_to_doc
			(Insert { at = pos 0 2; text = "ll" })
			d
		in
		assert (Doc.get_line 0 d = Some "hello"));

	test "apply Insert with newline splits line" (fun () ->
		let d = Doc.of_string "abcdef" in
		let d = Command.apply_to_doc
			(Insert { at = pos 0 3; text = "\n" })
			d
		in
		assert (Doc.line_count d = 2);
		assert (Doc.get_line 0 d = Some "abc");
		assert (Doc.get_line 1 d = Some "def"));

	test "apply Insert multi-line text" (fun () ->
		let d = Doc.of_string "ax" in
		let d = Command.apply_to_doc
			(Insert { at = pos 0 1; text = "b\nc\nd" })
			d
		in
		assert (Doc.line_count d = 3);
		assert (Doc.get_line 0 d = Some "ab");
		assert (Doc.get_line 1 d = Some "c");
		assert (Doc.get_line 2 d = Some "dx"));

	test "apply Delete within one line" (fun () ->
		let d = Doc.of_string "hello world" in
		let d = Command.apply_to_doc
			(Delete { start_pos = pos 0 5; end_pos = pos 0 11 })
			d
		in
		assert (Doc.get_line 0 d = Some "hello"));

	test "apply Delete across lines joins them" (fun () ->
		let d = Doc.of_string "hello\nworld" in
		let d = Command.apply_to_doc
			(Delete { start_pos = pos 0 5; end_pos = pos 1 0 })
			d
		in
		assert (Doc.line_count d = 1);
		assert (Doc.get_line 0 d = Some "helloworld"));

	test "apply Delete middle of multi-line" (fun () ->
		let d = Doc.of_string "abc\ndef\nghi" in
		let d = Command.apply_to_doc
			(Delete { start_pos = pos 0 1; end_pos = pos 2 2 })
			d
		in
		assert (Doc.line_count d = 1);
		assert (Doc.get_line 0 d = Some "ai"));

	test "apply Move_cursor is a no-op on doc" (fun () ->
		let d = Doc.of_string "a\nb" in
		let d' = Command.apply_to_doc (Move_cursor (pos 0 0)) d in
		assert (Doc.line_count d' = 2);
		assert (Doc.get_line 0 d' = Some "a"));

	test "apply Insert with cjk preserves grapheme positions" (fun () ->
		let d = Doc.of_string "\u{4E2D}\u{6587}" in
		let d = Command.apply_to_doc
			(Insert { at = pos 0 1; text = "X" })
			d
		in
		(* "中X文" *)
		assert (Doc.get_line 0 d = Some "\u{4E2D}X\u{6587}"));

	test "round-trip insert then delete" (fun () ->
		let d = Doc.of_string "hello" in
		let d = Command.apply_to_doc
			(Insert { at = pos 0 5; text = " world" })
			d
		in
		assert (Doc.get_line 0 d = Some "hello world");
		let d = Command.apply_to_doc
			(Delete { start_pos = pos 0 5; end_pos = pos 0 11 })
			d
		in
		assert (Doc.get_line 0 d = Some "hello"));

	print_endline "all command tests passed"
