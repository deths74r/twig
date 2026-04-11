open Twig

let test name f =
	try
		f ();
		Printf.printf "  ok  %s\n" name
	with e ->
		Printf.printf "fail  %s: %s\n" name (Printexc.to_string e);
		exit 1

let () =
	test "count empty" (fun () ->
		assert (Grapheme.count "" = 0));

	test "count ascii" (fun () ->
		assert (Grapheme.count "hello" = 5));

	test "count wide cjk" (fun () ->
		assert (Grapheme.count "\u{4E2D}\u{6587}" = 2));

	test "count combining mark cluster" (fun () ->
		(* e + combining acute = one grapheme cluster *)
		assert (Grapheme.count "e\u{0301}" = 1));

	test "count zwj emoji sequence" (fun () ->
		(* family: man + zwj + woman + zwj + girl + zwj + boy = one cluster *)
		assert (Grapheme.count "\u{1F468}\u{200D}\u{1F469}\u{200D}\u{1F467}\u{200D}\u{1F466}" = 1));

	test "byte_of_index ascii" (fun () ->
		assert (Grapheme.byte_of_index "hello" 0 = 0);
		assert (Grapheme.byte_of_index "hello" 3 = 3);
		assert (Grapheme.byte_of_index "hello" 5 = 5));

	test "byte_of_index clamps past end" (fun () ->
		assert (Grapheme.byte_of_index "abc" 100 = 3));

	test "byte_of_index clamps negative" (fun () ->
		assert (Grapheme.byte_of_index "abc" (-5) = 0));

	test "byte_of_index cjk" (fun () ->
		(* each cjk char is 3 bytes in utf-8 *)
		assert (Grapheme.byte_of_index "\u{4E2D}\u{6587}" 0 = 0);
		assert (Grapheme.byte_of_index "\u{4E2D}\u{6587}" 1 = 3);
		assert (Grapheme.byte_of_index "\u{4E2D}\u{6587}" 2 = 6));

	test "index_of_byte ascii" (fun () ->
		assert (Grapheme.index_of_byte "hello" 0 = 0);
		assert (Grapheme.index_of_byte "hello" 3 = 3));

	test "index_of_byte cjk" (fun () ->
		assert (Grapheme.index_of_byte "\u{4E2D}\u{6587}" 0 = 0);
		assert (Grapheme.index_of_byte "\u{4E2D}\u{6587}" 3 = 1);
		assert (Grapheme.index_of_byte "\u{4E2D}\u{6587}" 6 = 2));

	test "display_width ascii" (fun () ->
		assert (Grapheme.display_width "hello" = 5));

	test "display_width wide cjk" (fun () ->
		assert (Grapheme.display_width "\u{4E2D}\u{6587}" = 4));

	test "display_width combining mark" (fun () ->
		(* e + combining acute should be width 1 *)
		assert (Grapheme.display_width "e\u{0301}" = 1));

	test "display_width empty" (fun () ->
		assert (Grapheme.display_width "" = 0));

	test "next_word_start on ascii" (fun () ->
		assert (Grapheme.next_word_start "hello world" 0 = 6);
		assert (Grapheme.next_word_start "hello world" 3 = 6);
		assert (Grapheme.next_word_start "hello world" 5 = 6);
		assert (Grapheme.next_word_start "hello world" 6 = 11));

	test "next_word_start with leading spaces" (fun () ->
		assert (Grapheme.next_word_start "  hello" 0 = 2));

	test "next_word_start at end returns length" (fun () ->
		assert (Grapheme.next_word_start "hello" 5 = 5));

	test "next_word_start punctuation" (fun () ->
		assert (Grapheme.next_word_start "foo.bar" 0 = 4);
		assert (Grapheme.next_word_start "foo.bar" 3 = 4));

	test "prev_word_start on ascii" (fun () ->
		assert (Grapheme.prev_word_start "hello world" 11 = 6);
		assert (Grapheme.prev_word_start "hello world" 6 = 0);
		assert (Grapheme.prev_word_start "hello world" 5 = 0);
		assert (Grapheme.prev_word_start "hello world" 1 = 0));

	test "prev_word_start at zero stays at zero" (fun () ->
		assert (Grapheme.prev_word_start "abc" 0 = 0));

	test "prev_word_start with trailing space" (fun () ->
		assert (Grapheme.prev_word_start "hello " 6 = 0));

	print_endline "all grapheme tests passed"
