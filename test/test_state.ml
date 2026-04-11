open Twig

let test name f =
	try
		f ();
		Printf.printf "  ok  %s\n" name
	with e ->
		Printf.printf "fail  %s: %s\n" name (Printexc.to_string e);
		exit 1

let pos l c : Position.t = { line = l; column = c }

let state_of s =
	{ State.empty with doc = Doc.of_string s }

let () =
	test "empty state" (fun () ->
		let s = State.empty in
		assert (Doc.line_count s.doc = 0);
		assert (Position.equal s.cursor Position.origin);
		assert (not s.dirty);
		assert (not s.should_quit);
		assert (s.undo = []));

	test "apply Insert updates doc and cursor" (fun () ->
		let s = state_of "hello" in
		let s = State.apply
			(Insert { at = pos 0 5; text = " world" })
			s
		in
		assert (Doc.get_line 0 s.doc = Some "hello world");
		assert (Position.equal s.cursor (pos 0 11));
		assert (s.dirty));

	test "apply Insert with newline advances cursor to new line" (fun () ->
		let s = state_of "abc" in
		let s = State.apply
			(Insert { at = pos 0 3; text = "\ndef" })
			s
		in
		assert (Doc.line_count s.doc = 2);
		assert (Position.equal s.cursor (pos 1 3)));

	test "apply Delete updates cursor to start" (fun () ->
		let s = state_of "hello world" in
		let s = State.apply
			(Delete { start_pos = pos 0 5; end_pos = pos 0 11 })
			s
		in
		assert (Doc.get_line 0 s.doc = Some "hello");
		assert (Position.equal s.cursor (pos 0 5));
		assert (s.dirty));

	test "apply Move_cursor updates cursor" (fun () ->
		let s = state_of "hello\nworld" in
		let s = State.apply (Move_cursor (pos 1 3)) s in
		assert (Position.equal s.cursor (pos 1 3));
		assert (not s.dirty));

	test "Move_cursor clamps out-of-bounds line" (fun () ->
		let s = state_of "hello\nworld" in
		let s = State.apply (Move_cursor (pos 99 0)) s in
		assert (s.cursor.line = 1));

	test "Move_cursor clamps out-of-bounds column" (fun () ->
		let s = state_of "hi\nworld" in
		let s = State.apply (Move_cursor (pos 0 99)) s in
		assert (s.cursor.column = 2));

	test "Move_cursor clamps negative" (fun () ->
		let s = state_of "hello" in
		let s = State.apply (Move_cursor (pos (-5) (-5))) s in
		assert (Position.equal s.cursor Position.origin));

	test "Undo reverts Insert" (fun () ->
		let s = state_of "hello" in
		let s = State.apply (Move_cursor (pos 0 5)) s in
		let s = State.apply
			(Insert { at = pos 0 5; text = " world" })
			s
		in
		let s = State.apply Undo s in
		assert (Doc.get_line 0 s.doc = Some "hello");
		assert (Position.equal s.cursor (pos 0 5)));

	test "Redo reapplies" (fun () ->
		let s = state_of "hello" in
		let s = State.apply
			(Insert { at = pos 0 5; text = "!" })
			s
		in
		let s = State.apply Undo s in
		let s = State.apply Redo s in
		assert (Doc.get_line 0 s.doc = Some "hello!"));

	test "Undo on empty stack is no-op" (fun () ->
		let s = state_of "abc" in
		let s' = State.apply Undo s in
		assert (Doc.get_line 0 s'.doc = Some "abc"));

	test "new edit clears redo stack" (fun () ->
		let s = state_of "abc" in
		let s = State.apply (Insert { at = pos 0 3; text = "d" }) s in
		let s = State.apply Undo s in
		assert (s.redo <> []);
		let s = State.apply (Insert { at = pos 0 3; text = "X" }) s in
		assert (s.redo = []));

	test "burst groups contiguous inserts into one undo" (fun () ->
		let s = state_of "" in
		let s = State.apply (Insert { at = pos 0 0; text = "a" }) s in
		let s = State.apply (Insert { at = pos 0 1; text = "b" }) s in
		let s = State.apply (Insert { at = pos 0 2; text = "c" }) s in
		assert (Doc.get_line 0 s.doc = Some "abc");
		let s = State.apply Undo s in
		assert (Doc.line_count s.doc = 0));

	test "burst breaks on position jump" (fun () ->
		let s = state_of "hello" in
		let s = State.apply (Insert { at = pos 0 5; text = "X" }) s in
		let s = State.apply (Insert { at = pos 0 0; text = "Y" }) s in
		let s = State.apply Undo s in
		assert (Doc.get_line 0 s.doc = Some "helloX");
		let s = State.apply Undo s in
		assert (Doc.get_line 0 s.doc = Some "hello"));

	test "Move_cursor breaks insert burst" (fun () ->
		let s = state_of "" in
		let s = State.apply (Insert { at = pos 0 0; text = "a" }) s in
		let s = State.apply (Move_cursor (pos 0 1)) s in
		let s = State.apply (Insert { at = pos 0 1; text = "b" }) s in
		let s = State.apply Undo s in
		assert (Doc.get_line 0 s.doc = Some "a");
		let s = State.apply Undo s in
		assert (Doc.line_count s.doc = 0));

	test "Delete does not participate in bursts" (fun () ->
		let s = state_of "abcd" in
		let s = State.apply (Move_cursor (pos 0 4)) s in
		let s = State.apply
			(Delete { start_pos = pos 0 3; end_pos = pos 0 4 }) s
		in
		let s = State.apply
			(Delete { start_pos = pos 0 2; end_pos = pos 0 3 }) s
		in
		assert (Doc.get_line 0 s.doc = Some "ab");
		let s = State.apply Undo s in
		assert (Doc.get_line 0 s.doc = Some "abc");
		let s = State.apply Undo s in
		assert (Doc.get_line 0 s.doc = Some "abcd"));

	test "Quit sets should_quit" (fun () ->
		let s = state_of "abc" in
		let s = State.apply Quit s in
		assert (s.should_quit));

	test "Save writes and clears dirty" (fun () ->
		let path = Filename.temp_file "twig_test_" ".txt" in
		let s = {
			State.empty with
			doc = Doc.of_string "hello\nworld";
			filename = Some path;
			dirty = true;
		} in
		let s = State.apply Save s in
		assert (not s.dirty);
		let ic = open_in path in
		let content = really_input_string ic (in_channel_length ic) in
		close_in ic;
		Sys.remove path;
		assert (content = "hello\nworld"));

	test "search start records origin" (fun () ->
		let s = state_of "hello" in
		let s = State.apply (Move_cursor (pos 0 2)) s in
		let s = State.apply Search_start s in
		(match s.mode with
		| Searching search ->
			assert (search.query = "");
			assert (Position.equal search.origin (pos 0 2))
		| Edit -> assert false));

	test "search appends move cursor to match" (fun () ->
		let s = state_of "hello world" in
		let s = State.apply Search_start s in
		let s = State.apply (Search_append "wor") s in
		assert (Position.equal s.cursor (pos 0 6)));

	test "search with no match keeps cursor at origin" (fun () ->
		let s = state_of "hello" in
		let s = State.apply (Move_cursor (pos 0 1)) s in
		let s = State.apply Search_start s in
		let s = State.apply (Search_append "zzz") s in
		assert (Position.equal s.cursor (pos 0 1)));

	test "search backspace reverts to earlier match" (fun () ->
		let s = state_of "foo foobar" in
		let s = State.apply Search_start s in
		let s = State.apply (Search_append "foobar") s in
		assert (Position.equal s.cursor (pos 0 4));
		let s = State.apply Search_backspace s in
		let s = State.apply Search_backspace s in
		let s = State.apply Search_backspace s in
		assert (s.cursor.column = 0));

	test "search cancel restores cursor" (fun () ->
		let s = state_of "hello world" in
		let s = State.apply (Move_cursor (pos 0 3)) s in
		let s = State.apply Search_start s in
		let s = State.apply (Search_append "world") s in
		let s = State.apply Search_cancel s in
		assert (Position.equal s.cursor (pos 0 3));
		(match s.mode with
		| Edit -> ()
		| Searching _ -> assert false));

	test "search commit saves last_search" (fun () ->
		let s = state_of "hello world" in
		let s = State.apply Search_start s in
		let s = State.apply (Search_append "wor") s in
		let s = State.apply Search_commit s in
		assert (s.last_search = Some "wor");
		assert (Position.equal s.cursor (pos 0 6));
		(match s.mode with
		| Edit -> ()
		| Searching _ -> assert false));

	test "Search_next advances past current match" (fun () ->
		let s = state_of "foo foo foo" in
		let s = State.apply Search_start s in
		let s = State.apply (Search_append "foo") s in
		let s = State.apply Search_commit s in
		assert (s.cursor.column = 0);
		let s = State.apply Search_next s in
		assert (s.cursor.column = 4);
		let s = State.apply Search_next s in
		assert (s.cursor.column = 8));

	test "search wraps around" (fun () ->
		let s = state_of "alpha beta" in
		let s = State.apply (Move_cursor (pos 0 7)) s in
		let s = State.apply Search_start s in
		let s = State.apply (Search_append "alpha") s in
		assert (Position.equal s.cursor (pos 0 0)));

	test "Search_next with no last_search is a no-op" (fun () ->
		let s = state_of "hello" in
		let s' = State.apply Search_next s in
		assert (Position.equal s'.cursor s.cursor));

	test "of_file loads doc and sets filename" (fun () ->
		let path = Filename.temp_file "twig_test_" ".txt" in
		let oc = open_out path in
		output_string oc "line one\nline two\nline three";
		close_out oc;
		let s = State.of_file path in
		Sys.remove path;
		assert (Doc.line_count s.doc = 3);
		assert (s.filename = Some path);
		assert (not s.dirty));

	print_endline "all state tests passed"
