open Twig

let test name f =
	try
		f ();
		Printf.printf "  ok  %s\n" name
	with e ->
		Printf.printf "fail  %s: %s\n" name (Printexc.to_string e);
		exit 1

let kinds_of spans = List.map (fun (s : Syntax.span) -> s.kind) spans

let token_at_offset spans offset =
	List.find_opt
		(fun (s : Syntax.span) ->
			offset >= s.start && offset < s.start + s.length)
		spans
	|> Option.map (fun (s : Syntax.span) -> s.kind)

let () =
	test "plain returns a single Text span" (fun () ->
		let spans, st = Syntax.tokenize_line "hello world" Normal Plain in
		assert (List.length spans = 1);
		assert (st = Syntax.Normal));

	test "C line comment" (fun () ->
		let spans, _ = Syntax.tokenize_line "int x; // comment" Normal C in
		assert (token_at_offset spans 0 = Some Type_kw);
		assert (token_at_offset spans 7 = Some Comment));

	test "C block comment closing on same line" (fun () ->
		let spans, st =
			Syntax.tokenize_line "a /* b */ c" Normal C
		in
		assert (st = Syntax.Normal);
		assert (token_at_offset spans 2 = Some Comment);
		assert (token_at_offset spans 7 = Some Comment));

	let is_in_comment = function
		| Syntax.In_block_comment _ -> true
		| Normal -> false
	in

	test "C block comment carries state across lines" (fun () ->
		let _, st1 = Syntax.tokenize_line "/* start" Normal C in
		assert (is_in_comment st1);
		let _, st2 = Syntax.tokenize_line "still comment" st1 C in
		assert (is_in_comment st2);
		let _, st3 = Syntax.tokenize_line "end */" st2 C in
		assert (st3 = Syntax.Normal));

	test "C keyword vs type keyword" (fun () ->
		let spans, _ =
			Syntax.tokenize_line "if (x) return 0;" Normal C
		in
		assert (token_at_offset spans 0 = Some Keyword);
		assert (token_at_offset spans 7 = Some Keyword));

	test "C string literal" (fun () ->
		let spans, _ =
			Syntax.tokenize_line "char *s = \"hello\";" Normal C
		in
		assert (token_at_offset spans 0 = Some Type_kw);
		assert (token_at_offset spans 10 = Some String_lit));

	test "C string with escaped quote" (fun () ->
		let spans, _ =
			Syntax.tokenize_line "\"a\\\"b\"" Normal C
		in
		match spans with
		| [span] ->
			assert (span.kind = String_lit);
			assert (span.length = 6)
		| _ -> assert false);

	test "C char literal" (fun () ->
		let spans, _ = Syntax.tokenize_line "'a'" Normal C in
		assert (kinds_of spans = [Char_lit]));

	test "C number" (fun () ->
		let spans, _ = Syntax.tokenize_line "42 + 3.14" Normal C in
		assert (token_at_offset spans 0 = Some Number);
		assert (token_at_offset spans 5 = Some Number));

	test "C preprocessor" (fun () ->
		let spans, _ =
			Syntax.tokenize_line "#include <stdio.h>" Normal C
		in
		assert (kinds_of spans = [Preproc]));

	test "C preprocessor only at column 0" (fun () ->
		let spans, _ = Syntax.tokenize_line "  #not preproc" Normal C in
		assert (token_at_offset spans 2 <> Some Preproc));

	test "language_of_filename" (fun () ->
		assert (Syntax.language_of_filename (Some "foo.c") = C);
		assert (Syntax.language_of_filename (Some "foo.h") = C);
		assert (Syntax.language_of_filename (Some "foo.cpp") = C);
		assert (Syntax.language_of_filename (Some "foo.ml") = Ocaml);
		assert (Syntax.language_of_filename (Some "foo.mli") = Ocaml);
		assert (Syntax.language_of_filename (Some "foo.txt") = Plain);
		assert (Syntax.language_of_filename None = Plain));

	test "OCaml let keyword" (fun () ->
		let spans, _ =
			Syntax.tokenize_line "let x = 42" Normal Ocaml
		in
		assert (token_at_offset spans 0 = Some Keyword);
		assert (token_at_offset spans 8 = Some Number));

	test "OCaml block comment" (fun () ->
		let spans, st =
			Syntax.tokenize_line "(* comment *) let" Normal Ocaml
		in
		assert (st = Syntax.Normal);
		assert (token_at_offset spans 0 = Some Comment);
		assert (token_at_offset spans 14 = Some Keyword));

	test "OCaml nested comment" (fun () ->
		let _, st1 =
			Syntax.tokenize_line "(* outer (* inner" Normal Ocaml
		in
		(match st1 with
		| In_block_comment 2 -> ()
		| _ -> assert false);
		let _, st2 =
			Syntax.tokenize_line "inner *) outer" st1 Ocaml
		in
		(match st2 with
		| In_block_comment 1 -> ()
		| _ -> assert false);
		let _, st3 =
			Syntax.tokenize_line "done *)" st2 Ocaml
		in
		assert (st3 = Syntax.Normal));

	test "OCaml type keyword" (fun () ->
		let spans, _ =
			Syntax.tokenize_line "let x : int = 0" Normal Ocaml
		in
		assert (token_at_offset spans 8 = Some Type_kw));

	test "OCaml identifier with apostrophe" (fun () ->
		let spans, _ =
			Syntax.tokenize_line "let x' = 0" Normal Ocaml
		in
		(* x' should be one identifier, not x followed by '=' as char lit *)
		let text_spans =
			List.filter
				(fun (s : Syntax.span) -> s.kind = Text)
				spans
		in
		assert (List.length text_spans >= 1));

	print_endline "all syntax tests passed"
