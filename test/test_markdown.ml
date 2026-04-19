open Twig

let test name f =
	try
		f ();
		Printf.printf "  ok  %s\n" name
	with e ->
		Printf.printf "fail  %s: %s\n" name (Printexc.to_string e);
		exit 1

let theme_md = Theme.default.markdown

let tokenize ?(state = Markdown.init) line =
	Markdown.tokenize_line ~state ~line ~theme:theme_md

let covers_line spans line =
	let sl = String.length line in
	let rec check expected_start = function
		| [] -> expected_start = sl
		| (s : Markdown.span) :: rest ->
				if s.start <> expected_start then false
				else check s.stop rest
	in
	match spans with
	| [] -> sl = 0
	| _ -> check 0 spans

let reconstruct spans line =
	let buf = Buffer.create (String.length line) in
	List.iter (fun (s : Markdown.span) ->
		Buffer.add_substring buf line s.start (s.stop - s.start))
		spans;
	Buffer.contents buf

let () =
	(* ------- invariants across all inputs ------- *)

	test "spans partition the line contiguously" (fun () ->
		let inputs = [
			"plain text";
			"# heading";
			"**bold** text";
			"*italic*";
			"`code`";
			"~~strike~~";
			"[link](url)";
			"- item";
			"1. numbered";
			"> quoted";
			"---";
			"| a | b |";
			"";
			"mixed `code` and **bold**";
		] in
		List.iter (fun line ->
			let spans, _ = tokenize line in
			if not (covers_line spans line) then
				failwith (Printf.sprintf "not contiguous for %S" line);
			if reconstruct spans line <> line then
				failwith (Printf.sprintf "reconstruction mismatch for %S" line))
			inputs);

	(* ------- headings ------- *)

	test "heading level 1" (fun () ->
		let spans, _ = tokenize "# Title" in
		match spans with
		| [ s ] -> assert (s.style = theme_md.heading.(0))
		| _ -> failwith "expected single span for heading");

	test "heading level 6" (fun () ->
		let spans, _ = tokenize "###### six" in
		match spans with
		| [ s ] -> assert (s.style = theme_md.heading.(5))
		| _ -> failwith "expected single span");

	test "seven hashes is not a heading" (fun () ->
		let spans, _ = tokenize "####### too many" in
		(* Falls through to inline parsing → plain span *)
		match spans with
		| [ s ] -> assert (s.style = Theme.plain)
		| _ -> ());

	test "hash without space is not a heading" (fun () ->
		let spans, _ = tokenize "#tag" in
		match spans with
		| [ s ] -> assert (s.style = Theme.plain)
		| _ -> ());

	(* ------- horizontal rule ------- *)

	test "hr three dashes" (fun () ->
		let spans, _ = tokenize "---" in
		match spans with
		| [ s ] -> assert (s.style = theme_md.hr)
		| _ -> failwith "expected single hr span");

	test "hr three stars" (fun () ->
		let spans, _ = tokenize "***" in
		match spans with
		| [ s ] -> assert (s.style = theme_md.hr)
		| _ -> failwith "expected hr");

	test "hr requires at least 3 chars" (fun () ->
		let spans, _ = tokenize "--" in
		(* Not an hr; parses as plain/inline *)
		match spans with
		| [ s ] -> assert (s.style <> theme_md.hr)
		| _ -> ());

	(* ------- fenced code blocks ------- *)

	test "open fence enters code state" (fun () ->
		let _, st = tokenize "```ocaml" in
		assert (st.fenced_code = Some "ocaml"));

	test "open fence without lang stores empty tag" (fun () ->
		let _, st = tokenize "```" in
		assert (st.fenced_code = Some ""));

	test "line inside code block is styled as code" (fun () ->
		let state = { Markdown.fenced_code = Some "ocaml" } in
		let spans, st = tokenize ~state "let x = 1" in
		(match spans with
		 | [ s ] -> assert (s.style = theme_md.code_block)
		 | _ -> failwith "expected single span");
		assert (st.fenced_code = Some "ocaml"));

	test "closing fence exits code state" (fun () ->
		let state = { Markdown.fenced_code = Some "ocaml" } in
		let _, st = tokenize ~state "```" in
		assert (st.fenced_code = None));

	test "markdown ignored inside code block" (fun () ->
		let state = { Markdown.fenced_code = Some "" } in
		let spans, _ = tokenize ~state "**not bold here**" in
		match spans with
		| [ s ] -> assert (s.style = theme_md.code_block)
		| _ -> failwith "expected single code span");

	(* ------- bold / italic / code / strike ------- *)

	test "bold spans include the asterisks" (fun () ->
		let spans, _ = tokenize "pre **bold** post" in
		let bolds =
			List.filter (fun (s : Markdown.span) ->
				s.style = theme_md.bold) spans
		in
		assert (List.length bolds = 1);
		let b = List.hd bolds in
		(* "**bold**" in "pre **bold** post" starts at offset 4 *)
		assert (b.start = 4);
		assert (b.stop = 12));

	test "italic with asterisks" (fun () ->
		let spans, _ = tokenize "*italic*" in
		let italics =
			List.filter (fun (s : Markdown.span) ->
				s.style = theme_md.italic) spans
		in
		assert (List.length italics = 1));

	test "italic with underscores" (fun () ->
		let spans, _ = tokenize "_italic_" in
		let italics =
			List.filter (fun (s : Markdown.span) ->
				s.style = theme_md.italic) spans
		in
		assert (List.length italics = 1));

	test "inline code" (fun () ->
		let spans, _ = tokenize "use `foo` please" in
		let codes =
			List.filter (fun (s : Markdown.span) ->
				s.style = theme_md.inline_code) spans
		in
		assert (List.length codes = 1));

	test "strikethrough" (fun () ->
		let spans, _ = tokenize "~~gone~~" in
		let strikes =
			List.filter (fun (s : Markdown.span) ->
				s.style = theme_md.strikethrough) spans
		in
		assert (List.length strikes = 1));

	test "unmatched bold degrades to plain" (fun () ->
		let spans, _ = tokenize "**no closer" in
		List.iter (fun (s : Markdown.span) ->
			if s.style = theme_md.bold then
				failwith "bold should not match without closer")
			spans);

	(* ------- links ------- *)

	test "link covers bracket to paren" (fun () ->
		let spans, _ = tokenize "see [here](http://x)" in
		let links =
			List.filter (fun (s : Markdown.span) ->
				s.style = theme_md.link) spans
		in
		assert (List.length links = 1);
		let l = List.hd links in
		assert (l.start = 4);
		(* "[here](http://x)" is 16 chars starting at 4 *)
		assert (l.stop = 20));

	(* ------- blockquote ------- *)

	test "blockquote styled whole-line" (fun () ->
		let spans, _ = tokenize "> quoted" in
		match spans with
		| [ s ] -> assert (s.style = theme_md.blockquote)
		| _ -> failwith "expected single blockquote span");

	(* ------- lists ------- *)

	test "bullet list marker separates from body" (fun () ->
		let spans, _ = tokenize "- item body" in
		match spans with
		| marker :: _ ->
				assert (marker.style = theme_md.list_marker);
				assert (marker.start = 0);
				assert (marker.stop = 2)  (* "- " *)
		| [] -> failwith "expected spans");

	test "numbered list marker" (fun () ->
		let spans, _ = tokenize "42. item" in
		match spans with
		| marker :: _ ->
				assert (marker.style = theme_md.list_marker);
				assert (marker.stop = 4)  (* "42. " *)
		| [] -> failwith "expected spans");

	test "star bullet recognised" (fun () ->
		let spans, _ = tokenize "* starred" in
		match spans with
		| marker :: _ ->
				assert (marker.style = theme_md.list_marker)
		| [] -> failwith "expected spans");

	test "list with bold body" (fun () ->
		let spans, _ = tokenize "- **strong** item" in
		let bolds =
			List.filter (fun (s : Markdown.span) ->
				s.style = theme_md.bold) spans
		in
		assert (List.length bolds = 1));

	(* ------- tables ------- *)

	test "table row styles pipes" (fun () ->
		let spans, _ = tokenize "| a | b |" in
		let pipes =
			List.filter (fun (s : Markdown.span) ->
				s.style = theme_md.table_border) spans
		in
		(* Three pipes *)
		assert (List.length pipes = 3));

	test "table row cell content is plain" (fun () ->
		let spans, _ = tokenize "| a | b |" in
		let plains =
			List.filter (fun (s : Markdown.span) ->
				s.style = Theme.plain) spans
		in
		(* cell gaps: between pipe 0 and pipe 1, between pipe 1 and 2 *)
		assert (List.length plains >= 2));

	(* ------- edge cases ------- *)

	test "empty line produces a single zero-width plain span" (fun () ->
		let spans, _ = tokenize "" in
		match spans with
		| [ s ] ->
				assert (s.start = 0);
				assert (s.stop = 0);
				assert (s.style = Theme.plain)
		| _ -> failwith "expected single span");

	test "init state has no fenced code" (fun () ->
		assert (Markdown.init.fenced_code = None));

	print_endline "test_markdown done"
