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
		| Edit | Opening_file _ | Command_chord | Command_prompt _ -> assert false));

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
		| Searching _ | Opening_file _ | Command_chord | Command_prompt _ -> assert false));

	test "search commit saves last_search" (fun () ->
		let s = state_of "hello world" in
		let s = State.apply Search_start s in
		let s = State.apply (Search_append "wor") s in
		let s = State.apply Search_commit s in
		assert (s.last_search = Some "wor");
		assert (Position.equal s.cursor (pos 0 6));
		(match s.mode with
		| Edit -> ()
		| Searching _ | Opening_file _ | Command_chord | Command_prompt _ -> assert false));

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

	test "Set_mark records cursor position" (fun () ->
		let s = state_of "hello" in
		let s = State.apply (Move_cursor (pos 0 2)) s in
		let s = State.apply Set_mark s in
		assert (s.mark = Some (pos 0 2)));

	test "Move_cursor clears mark" (fun () ->
		let s = state_of "hello" in
		let s = State.apply Set_mark s in
		let s = State.apply (Move_cursor (pos 0 3)) s in
		assert (s.mark = None));

	test "Extend_cursor sets mark if unset then moves" (fun () ->
		let s = state_of "hello" in
		let s = State.apply (Move_cursor (pos 0 1)) s in
		let s = State.apply (Extend_cursor (pos 0 4)) s in
		assert (s.mark = Some (pos 0 1));
		assert (Position.equal s.cursor (pos 0 4)));

	test "Extend_cursor preserves existing mark" (fun () ->
		let s = state_of "hello" in
		let s = State.apply (Move_cursor (pos 0 0)) s in
		let s = State.apply Set_mark s in
		let s = State.apply (Extend_cursor (pos 0 2)) s in
		let s = State.apply (Extend_cursor (pos 0 4)) s in
		assert (s.mark = Some (pos 0 0)));

	test "Copy saves selection to yank" (fun () ->
		let s = state_of "hello world" in
		let s = State.apply (Move_cursor (pos 0 6)) s in
		let s = State.apply Set_mark s in
		let s = State.apply (Move_cursor (pos 0 11)) s in
		(* Move_cursor clears mark — use Extend *)
		let s = { s with mark = Some (pos 0 6) } in
		let s = State.apply Copy s in
		assert (s.yank = Some "world");
		assert (s.mark = None));

	test "Copy with reversed selection still works" (fun () ->
		let s = state_of "hello" in
		let s = { s with mark = Some (pos 0 4); cursor = pos 0 1 } in
		let s = State.apply Copy s in
		assert (s.yank = Some "ell"));

	test "Cut removes selection and yanks it" (fun () ->
		let s = state_of "hello world" in
		let s = { s with mark = Some (pos 0 5); cursor = pos 0 11 } in
		let s = State.apply Cut s in
		assert (s.yank = Some " world");
		assert (Doc.get_line 0 s.doc = Some "hello");
		assert (Position.equal s.cursor (pos 0 5)));

	test "Paste inserts yank at cursor" (fun () ->
		let s = state_of "hello" in
		let s = { s with yank = Some " world"; cursor = pos 0 5 } in
		let s = State.apply Paste s in
		assert (Doc.get_line 0 s.doc = Some "hello world");
		assert (Position.equal s.cursor (pos 0 11)));

	test "Paste with no yank is a no-op" (fun () ->
		let s = state_of "abc" in
		let s = State.apply Paste s in
		assert (Doc.get_line 0 s.doc = Some "abc"));

	test "Cut across lines joins them" (fun () ->
		let s = state_of "hello\nworld" in
		let s = { s with mark = Some (pos 0 5); cursor = pos 1 0 } in
		let s = State.apply Cut s in
		assert (Doc.line_count s.doc = 1);
		assert (Doc.get_line 0 s.doc = Some "helloworld");
		assert (s.yank = Some "\n"));

	test "Paste multi-line inserts new lines" (fun () ->
		let s = state_of "abc" in
		let s = { s with yank = Some "X\nY"; cursor = pos 0 1 } in
		let s = State.apply Paste s in
		assert (Doc.line_count s.doc = 2);
		assert (Doc.get_line 0 s.doc = Some "aX");
		assert (Doc.get_line 1 s.doc = Some "Ybc"));

	test "Cut and Paste roundtrip preserves text" (fun () ->
		let s = state_of "hello world" in
		let s = { s with mark = Some (pos 0 6); cursor = pos 0 11 } in
		let s = State.apply Cut s in
		let s = State.apply (Move_cursor (pos 0 0)) s in
		let s = State.apply Paste s in
		assert (Doc.get_line 0 s.doc = Some "worldhello "));

	test "Insert with mark replaces selection" (fun () ->
		let s = state_of "hello world" in
		let s = { s with mark = Some (pos 0 6); cursor = pos 0 11 } in
		let s = State.apply
			(Insert { at = pos 0 11; text = "you" }) s
		in
		assert (Doc.get_line 0 s.doc = Some "hello you");
		assert (Position.equal s.cursor (pos 0 9));
		assert (s.mark = None));

	test "Insert_newline with mark replaces selection" (fun () ->
		let s = state_of "hello world" in
		let s = { s with mark = Some (pos 0 5); cursor = pos 0 11 } in
		let s = State.apply Insert_newline s in
		assert (Doc.line_count s.doc = 2);
		assert (Doc.get_line 0 s.doc = Some "hello");
		assert (Doc.get_line 1 s.doc = Some "");
		assert (s.mark = None));

	test "Paste with mark replaces selection" (fun () ->
		let s = state_of "hello world" in
		let s = { s with
			mark = Some (pos 0 6);
			cursor = pos 0 11;
			yank = Some "friend";
		} in
		let s = State.apply Paste s in
		assert (Doc.get_line 0 s.doc = Some "hello friend");
		assert (s.mark = None));

	test "Insert_newline auto-indents from current line" (fun () ->
		let s = state_of "\thello" in
		let s = State.apply (Move_cursor (pos 0 6)) s in
		let s = State.apply Insert_newline s in
		assert (Doc.line_count s.doc = 2);
		assert (Doc.get_line 0 s.doc = Some "\thello");
		assert (Doc.get_line 1 s.doc = Some "\t");
		assert (Position.equal s.cursor (pos 1 1)));

	test "Insert_newline preserves space indent" (fun () ->
		let s = state_of "    four" in
		let s = State.apply (Move_cursor (pos 0 8)) s in
		let s = State.apply Insert_newline s in
		assert (Doc.get_line 1 s.doc = Some "    ");
		assert (Position.equal s.cursor (pos 1 4)));

	test "Insert_newline skips auto-indent when cursor is inside leading ws" (fun () ->
		let s = state_of "    hello" in
		let s = State.apply (Move_cursor (pos 0 2)) s in
		let s = State.apply Insert_newline s in
		assert (Doc.get_line 0 s.doc = Some "  ");
		assert (Doc.get_line 1 s.doc = Some "  hello"));

	test "Insert_newline on empty line adds empty line" (fun () ->
		let s = state_of "" in
		let s = State.apply Insert_newline s in
		assert (Doc.line_count s.doc = 2);
		assert (Position.equal s.cursor (pos 1 0)));

	test "Indent_block shifts every line in range" (fun () ->
		let s = state_of "a\nb\nc" in
		let s = { s with mark = Some (pos 0 0); cursor = pos 2 0 } in
		let s = State.apply Indent_block s in
		assert (Doc.get_line 0 s.doc = Some "\ta");
		assert (Doc.get_line 1 s.doc = Some "\tb");
		assert (Doc.get_line 2 s.doc = Some "\tc"));

	test "Indent_block keeps cursor and mark aligned to content" (fun () ->
		let s = state_of "abc\ndef" in
		let s = { s with mark = Some (pos 0 1); cursor = pos 1 2 } in
		let s = State.apply Indent_block s in
		assert (Position.equal s.cursor (pos 1 3));
		assert (s.mark = Some (pos 0 2)));

	test "Outdent_block removes leading tab" (fun () ->
		let s = state_of "\ta\n\tb" in
		let s = { s with mark = Some (pos 0 0); cursor = pos 1 1 } in
		let s = State.apply Outdent_block s in
		assert (Doc.get_line 0 s.doc = Some "a");
		assert (Doc.get_line 1 s.doc = Some "b"));

	test "Outdent_block removes up to 8 spaces" (fun () ->
		let s = state_of "        a" in
		let s = { s with cursor = pos 0 8 } in
		let s = State.apply Outdent_block s in
		assert (Doc.get_line 0 s.doc = Some "a");
		assert (s.cursor.column = 0));

	test "Outdent_block without mark outdents cursor's line only" (fun () ->
		let s = state_of "\ta\n\tb" in
		let s = { s with cursor = pos 0 1 } in
		let s = State.apply Outdent_block s in
		assert (Doc.get_line 0 s.doc = Some "a");
		assert (Doc.get_line 1 s.doc = Some "\tb"));

	test "Outdent_block is a no-op on a line with no leading ws" (fun () ->
		let s = state_of "hello" in
		let s = State.apply Outdent_block s in
		assert (Doc.get_line 0 s.doc = Some "hello"));

	test "Enter_command_chord enters Command_chord" (fun () ->
		let s = state_of "hello" in
		let s = State.apply Enter_command_chord s in
		(match s.mode with
		| Command_chord -> ()
		| _ -> assert false));

	test "Enter_command_prompt opens prompt with prefix" (fun () ->
		let s = state_of "" in
		let s = State.apply (Enter_command_prompt "goto ") s in
		(match s.mode with
		| Command_prompt cp -> assert (cp.input = "goto ")
		| _ -> assert false));

	test "Command_input appends to prompt buffer" (fun () ->
		let s = state_of "" in
		let s = State.apply (Enter_command_prompt "") s in
		let s = State.apply (Command_input "go") s in
		let s = State.apply (Command_input "to") s in
		(match s.mode with
		| Command_prompt cp -> assert (cp.input = "goto")
		| _ -> assert false));

	test "Command_backspace on empty prompt cancels" (fun () ->
		let s = state_of "" in
		let s = State.apply (Enter_command_prompt "") s in
		let s = State.apply Command_backspace s in
		(match s.mode with
		| Edit -> ()
		| _ -> assert false));

	test "Command_cancel returns to Edit from chord" (fun () ->
		let s = state_of "" in
		let s = State.apply Enter_command_chord s in
		let s = State.apply Command_cancel s in
		(match s.mode with
		| Edit -> ()
		| _ -> assert false));

	test "Command_cancel returns to Edit from prompt" (fun () ->
		let s = state_of "" in
		let s = State.apply (Enter_command_prompt "quit") s in
		let s = State.apply Command_cancel s in
		(match s.mode with
		| Edit -> ()
		| _ -> assert false);
		assert (not s.should_quit));

	test "Command quit on clean buffer sets should_quit" (fun () ->
		let s = state_of "hello" in
		let s = State.apply (Enter_command_prompt "") s in
		let s = State.apply (Command_input "q") s in
		let s = State.apply Command_execute s in
		assert (s.should_quit));

	test "Command quit on dirty buffer refuses" (fun () ->
		let s = { (state_of "hello") with dirty = true } in
		let s = State.apply (Enter_command_prompt "") s in
		let s = State.apply (Command_input "quit") s in
		let s = State.apply Command_execute s in
		assert (not s.should_quit);
		assert (s.message <> None));

	test "Command q! forces quit even when dirty" (fun () ->
		let s = { (state_of "hello") with dirty = true } in
		let s = State.apply (Enter_command_prompt "") s in
		let s = State.apply (Command_input "q!") s in
		let s = State.apply Command_execute s in
		assert (s.should_quit));

	test "Command goto jumps to line" (fun () ->
		let s = state_of "aaa\nbbb\nccc\nddd" in
		let s = State.apply (Enter_command_prompt "") s in
		let s = State.apply (Command_input "goto 3") s in
		let s = State.apply Command_execute s in
		assert (s.cursor.line = 2));

	test "Goto preview highlights target line" (fun () ->
		let s = state_of "aaa\nbbb\nccc" in
		let s = State.apply (Enter_command_prompt "goto ") s in
		let s = State.apply (Command_input "2") s in
		(match s.mode with
		| Command_prompt cp -> assert (cp.preview_line = Some 1)
		| _ -> assert false));

	test "Command top jumps to start" (fun () ->
		let s = state_of "aaa\nbbb\nccc" in
		let s = State.apply (Move_cursor (pos 2 0)) s in
		let s = State.apply (Enter_command_prompt "") s in
		let s = State.apply (Command_input "top") s in
		let s = State.apply Command_execute s in
		assert (Position.equal s.cursor Position.origin));

	test "Command bottom jumps to end" (fun () ->
		let s = state_of "aaa\nbbb\nccc" in
		let s = State.apply (Enter_command_prompt "") s in
		let s = State.apply (Command_input "bottom") s in
		let s = State.apply Command_execute s in
		assert (s.cursor.line = 2));

	test "Command save as changes filename" (fun () ->
		let path = Filename.temp_file "twig_saveas_" ".txt" in
		let s = state_of "saved content" in
		let s = State.apply (Enter_command_prompt "") s in
		let s = State.apply (Command_input ("save as " ^ path)) s in
		let s = State.apply Command_execute s in
		assert (s.filename = Some path);
		assert (not s.dirty);
		let ic = open_in path in
		let content = really_input_string ic (in_channel_length ic) in
		close_in ic;
		Sys.remove path;
		assert (content = "saved content"));

	test "Command replace substitutes globally" (fun () ->
		let s = state_of "foo bar foo baz foo" in
		let s = State.apply (Enter_command_prompt "") s in
		let s = State.apply (Command_input "replace foo with X") s in
		let s = State.apply Command_execute s in
		assert (Doc.get_line 0 s.doc = Some "X bar X baz X");
		assert (s.dirty));

	test "Command replace across lines" (fun () ->
		let s = state_of "hello world\nhello again" in
		let s = State.apply (Enter_command_prompt "") s in
		let s = State.apply (Command_input "replace hello with hi") s in
		let s = State.apply Command_execute s in
		assert (Doc.get_line 0 s.doc = Some "hi world");
		assert (Doc.get_line 1 s.doc = Some "hi again"));

	test "Command replace no matches" (fun () ->
		let s = state_of "abc" in
		let s = State.apply (Enter_command_prompt "") s in
		let s = State.apply (Command_input "replace zzz with q") s in
		let s = State.apply Command_execute s in
		assert (Doc.get_line 0 s.doc = Some "abc");
		assert (s.message <> None));

	test "Command dup duplicates current line" (fun () ->
		let s = state_of "hello\nworld" in
		let s = State.apply (Enter_command_prompt "") s in
		let s = State.apply (Command_input "dup") s in
		let s = State.apply Command_execute s in
		assert (Doc.line_count s.doc = 3);
		assert (Doc.get_line 0 s.doc = Some "hello");
		assert (Doc.get_line 1 s.doc = Some "hello");
		assert (Doc.get_line 2 s.doc = Some "world"));

	test "Command dup N duplicates N times" (fun () ->
		let s = state_of "line" in
		let s = State.apply (Enter_command_prompt "") s in
		let s = State.apply (Command_input "dup 3") s in
		let s = State.apply Command_execute s in
		assert (Doc.line_count s.doc = 4));

	test "Command theme cycles" (fun () ->
		let s = state_of "" in
		assert (s.theme_name = "default");
		let s = State.apply (Enter_command_prompt "") s in
		let s = State.apply (Command_input "theme") s in
		let s = State.apply Command_execute s in
		assert (s.theme_name <> "default"));

	test "Command theme by name" (fun () ->
		let s = state_of "" in
		let s = State.apply (Enter_command_prompt "") s in
		let s = State.apply (Command_input "theme solar") s in
		let s = State.apply Command_execute s in
		assert (s.theme_name = "solar"));

	test "Command theme invalid name shows error" (fun () ->
		let s = state_of "" in
		let s = State.apply (Enter_command_prompt "") s in
		let s = State.apply (Command_input "theme bogus") s in
		let s = State.apply Command_execute s in
		assert (s.theme_name = "default");
		assert (s.message <> None));

	test "Command help shows message" (fun () ->
		let s = state_of "" in
		let s = State.apply (Enter_command_prompt "") s in
		let s = State.apply (Command_input "help") s in
		let s = State.apply Command_execute s in
		assert (s.message <> None));

	test "Unknown command shows error message" (fun () ->
		let s = state_of "" in
		let s = State.apply (Enter_command_prompt "") s in
		let s = State.apply (Command_input "foobar") s in
		let s = State.apply Command_execute s in
		assert (s.message <> None));

	test "Open_file_start enters Opening_file mode" (fun () ->
		let s = state_of "hello" in
		let s = State.apply Open_file_start s in
		(match s.mode with
		| Opening_file of_state -> assert (of_state.path = "")
		| _ -> assert false));

	test "Open_file_start prefills dirname of current file" (fun () ->
		let s = { State.empty with filename = Some "/tmp/foo/bar.txt" } in
		let s = State.apply Open_file_start s in
		(match s.mode with
		| Opening_file of_state -> assert (of_state.path = "/tmp/foo/")
		| _ -> assert false));

	test "Open_file_append extends path" (fun () ->
		let s = State.apply Open_file_start State.empty in
		let s = State.apply (Open_file_append "a") s in
		let s = State.apply (Open_file_append "bc") s in
		(match s.mode with
		| Opening_file of_state -> assert (of_state.path = "abc")
		| _ -> assert false));

	test "Open_file_backspace shrinks path" (fun () ->
		let s = State.apply Open_file_start State.empty in
		let s = State.apply (Open_file_append "hello") s in
		let s = State.apply Open_file_backspace s in
		(match s.mode with
		| Opening_file of_state -> assert (of_state.path = "hell")
		| _ -> assert false));

	test "Open_file_cancel returns to Edit without loading" (fun () ->
		let s = state_of "original" in
		let s = State.apply Open_file_start s in
		let s = State.apply (Open_file_append "nonexistent") s in
		let s = State.apply Open_file_cancel s in
		(match s.mode with
		| Edit -> ()
		| _ -> assert false);
		assert (Doc.get_line 0 s.doc = Some "original"));

	test "Open_file_commit refuses if dirty" (fun () ->
		let path = Filename.temp_file "twig_test_" ".txt" in
		let oc = open_out path in
		output_string oc "from disk";
		close_out oc;
		let s = { State.empty with
			doc = Doc.of_string "unsaved";
			dirty = true;
		} in
		let s = State.apply Open_file_start s in
		let s = State.apply (Open_file_append path) s in
		let s = State.apply Open_file_commit s in
		Sys.remove path;
		(match s.mode with
		| Edit -> ()
		| _ -> assert false);
		assert (Doc.get_line 0 s.doc = Some "unsaved");
		assert (s.message <> None));

	test "Open_file_commit loads file when clean" (fun () ->
		let path = Filename.temp_file "twig_test_" ".txt" in
		let oc = open_out path in
		output_string oc "line1\nline2";
		close_out oc;
		let s = State.apply Open_file_start State.empty in
		let s = State.apply (Open_file_append path) s in
		let s = State.apply Open_file_commit s in
		Sys.remove path;
		(match s.mode with
		| Edit -> ()
		| _ -> assert false);
		assert (Doc.line_count s.doc = 2);
		assert (s.filename = Some path));

	test "Open_file_commit creates new file for missing path" (fun () ->
		let path = "/tmp/twig_definitely_does_not_exist_xyz.txt" in
		let s = State.apply Open_file_start State.empty in
		let s = State.apply (Open_file_append path) s in
		let s = State.apply Open_file_commit s in
		(match s.mode with
		| Edit -> ()
		| _ -> assert false);
		assert (s.filename = Some path);
		assert (Doc.line_count s.doc = 0));

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
