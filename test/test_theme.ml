open Twig

let test name f =
	try
		f ();
		Printf.printf "  ok  %s\n" name
	with e ->
		Printf.printf "fail  %s: %s\n" name (Printexc.to_string e);
		exit 1

let () =
	(* ------- parse_color ------- *)

	test "parse_color default" (fun () ->
		match Theme.parse_color "default" with
		| Ok Theme.Default -> ()
		| _ -> failwith "expected Default");

	test "parse_color dash" (fun () ->
		match Theme.parse_color "-" with
		| Ok Theme.Default -> ()
		| _ -> failwith "expected Default");

	test "parse_color named red" (fun () ->
		match Theme.parse_color "red" with
		| Ok (Theme.Ansi 1) -> ()
		| _ -> failwith "expected Ansi 1");

	test "parse_color named bright_blue" (fun () ->
		match Theme.parse_color "bright_blue" with
		| Ok (Theme.Ansi 12) -> ()
		| _ -> failwith "expected Ansi 12");

	test "parse_color integer 0..15" (fun () ->
		match Theme.parse_color "7" with
		| Ok (Theme.Ansi 7) -> ()
		| _ -> failwith "expected Ansi 7");

	test "parse_color integer 16..255" (fun () ->
		match Theme.parse_color "141" with
		| Ok (Theme.Ansi256 141) -> ()
		| _ -> failwith "expected Ansi256 141");

	test "parse_color integer 256 errors" (fun () ->
		match Theme.parse_color "256" with
		| Error _ -> ()
		| Ok _ -> failwith "expected error for 256");

	test "parse_color hex" (fun () ->
		match Theme.parse_color "#ff8000" with
		| Ok (Theme.Rgb (255, 128, 0)) -> ()
		| _ -> failwith "expected Rgb (255,128,0)");

	test "parse_color bad hex errors" (fun () ->
		match Theme.parse_color "#xyz123" with
		| Error _ -> ()
		| Ok _ -> failwith "expected error on bad hex");

	test "parse_color gibberish errors" (fun () ->
		match Theme.parse_color "notacolor" with
		| Error _ -> ()
		| Ok _ -> failwith "expected error on unknown");

	test "parse_color case-insensitive named" (fun () ->
		match Theme.parse_color "RED" with
		| Ok (Theme.Ansi 1) -> ()
		| _ -> failwith "expected Ansi 1");

	(* ------- style_to_ansi ------- *)

	test "style_to_ansi plain is empty" (fun () ->
		assert (Theme.style_to_ansi Theme.plain = ""));

	test "style_to_ansi bold only" (fun () ->
		let st = { Theme.plain with bold = true } in
		assert (Theme.style_to_ansi st = "\x1b[1m"));

	test "style_to_ansi fg red" (fun () ->
		let st = { Theme.plain with fg = Theme.Ansi 1 } in
		assert (Theme.style_to_ansi st = "\x1b[31m"));

	test "style_to_ansi fg ansi256" (fun () ->
		let st = { Theme.plain with fg = Theme.Ansi256 141 } in
		assert (Theme.style_to_ansi st = "\x1b[38;5;141m"));

	test "style_to_ansi fg rgb" (fun () ->
		let st = { Theme.plain with fg = Theme.Rgb (255, 128, 0) } in
		assert (Theme.style_to_ansi st = "\x1b[38;2;255;128;0m"));

	test "style_to_ansi bg ansi" (fun () ->
		let st = { Theme.plain with bg = Theme.Ansi 4 } in
		assert (Theme.style_to_ansi st = "\x1b[44m"));

	test "style_to_ansi bold + fg" (fun () ->
		let st = { Theme.plain with bold = true; fg = Theme.Ansi 5 } in
		assert (Theme.style_to_ansi st = "\x1b[1;35m"));

	test "style_to_ansi multiple flags combined" (fun () ->
		let st = {
			Theme.plain with
			bold = true;
			italic = true;
			underline = true;
			fg = Theme.Ansi256 141;
		} in
		assert (Theme.style_to_ansi st = "\x1b[1;3;4;38;5;141m"));

	test "style_to_ansi bright ansi fg" (fun () ->
		let st = { Theme.plain with fg = Theme.Ansi 8 } in
		assert (Theme.style_to_ansi st = "\x1b[90m"));

	test "reset constant" (fun () ->
		assert (Theme.reset = "\x1b[0m"));

	(* ------- default theme ------- *)

	test "default theme has all 6 heading slots" (fun () ->
		assert (Array.length Theme.default.markdown.heading = 6));

	test "default theme has 4 bracket colors" (fun () ->
		assert (Array.length Theme.default.syntax.bracket_colors = 4));

	test "default keyword has bold + magenta (twig-edit parity)" (fun () ->
		let k = Theme.default.syntax.keyword in
		assert (k.bold = true);
		match k.fg with
		| Theme.Ansi 5 -> ()
		| _ -> failwith "expected Ansi 5 magenta");

	test "default comment is bright_black" (fun () ->
		let c = Theme.default.syntax.comment in
		match c.fg with
		| Theme.Ansi 8 -> ()
		| _ -> failwith "expected Ansi 8");

	(* ------- load_string ------- *)

	test "load_string empty returns default" (fun () ->
		match Theme.load_string "" with
		| Ok t ->
				assert (t.syntax.keyword = Theme.default.syntax.keyword);
				assert (t.chrome.title_focused
				        = Theme.default.chrome.title_focused)
		| Error (`Parse_error msg) ->
				failwith ("unexpected parse error: " ^ msg));

	test "load_string overrides one field" (fun () ->
		let src =
			"[syntax.keyword]\nfg = \"blue\"\nbold = false\n"
		in
		match Theme.load_string src with
		| Ok t ->
				assert (t.syntax.keyword.fg = Theme.Ansi 4);
				assert (t.syntax.keyword.bold = false);
				(* other fields stay default *)
				assert (t.syntax.comment = Theme.default.syntax.comment)
		| Error (`Parse_error msg) ->
				failwith ("unexpected parse error: " ^ msg));

	test "load_string bad color reports error" (fun () ->
		let src = "[syntax.keyword]\nfg = \"notacolor\"\n" in
		match Theme.load_string src with
		| Error (`Parse_error msg) ->
				(* Should mention the problem *)
				assert (String.length msg > 0)
		| Ok _ -> failwith "expected parse error");

	test "load_string malformed TOML reports error" (fun () ->
		let src = "[syntax.keyword\nfg = " in
		match Theme.load_string src with
		| Error (`Parse_error _) -> ()
		| Ok _ -> failwith "expected parse error");

	test "load_string override heading3" (fun () ->
		let src =
			"[markdown.heading3]\nfg = \"white\"\nbold = false\n"
		in
		match Theme.load_string src with
		| Ok t ->
				let h3 = t.markdown.heading.(2) in
				assert (h3.fg = Theme.Ansi 7);
				assert (h3.bold = false)
		| Error (`Parse_error msg) ->
				failwith ("unexpected parse error: " ^ msg));

	test "load_string override bracket_color2" (fun () ->
		let src =
			"[syntax.bracket_color2]\nfg = \"#abcdef\"\n"
		in
		match Theme.load_string src with
		| Ok t ->
				let b2 = t.syntax.bracket_colors.(1) in
				assert (b2.fg = Theme.Rgb (0xab, 0xcd, 0xef))
		| Error (`Parse_error msg) ->
				failwith ("unexpected parse error: " ^ msg));

	(* The bundled default.toml / Theme.default parity check lands in
	   0c-ii when twig-edit starts consuming the theme via TOML. At
	   that point a visual-parity test has a concrete consumer; here
	   it would only verify that two hand-written values agree. *)

	print_endline "test_theme done"
