open Twig

let test name f =
	try
		f ();
		Printf.printf "  ok  %s\n" name
	with e ->
		Printf.printf "fail  %s: %s\n" name (Printexc.to_string e);
		exit 1

let rect_eq (a : Rect.t) (b : Rect.t) =
	a.row = b.row && a.col = b.col
	&& a.rows = b.rows && a.cols = b.cols

let show (r : Rect.t) =
	Printf.sprintf "{row=%d; col=%d; rows=%d; cols=%d}"
		r.row r.col r.rows r.cols

let assert_rect ~expected ~actual =
	if not (rect_eq expected actual) then
		failwith
			(Printf.sprintf "expected %s, got %s"
				(show expected) (show actual))

let () =
	(* ------- make / empty / is_empty ------- *)

	test "make with all positive" (fun () ->
		let r = Rect.make ~row:1 ~col:2 ~rows:3 ~cols:4 in
		assert (r.row = 1 && r.col = 2 && r.rows = 3 && r.cols = 4));

	test "make clamps negative rows to 0" (fun () ->
		let r = Rect.make ~row:0 ~col:0 ~rows:(-5) ~cols:10 in
		assert (r.rows = 0 && r.cols = 10));

	test "make clamps negative cols to 0" (fun () ->
		let r = Rect.make ~row:0 ~col:0 ~rows:10 ~cols:(-5) in
		assert (r.rows = 10 && r.cols = 0));

	test "zero rows is empty" (fun () ->
		let r = Rect.make ~row:0 ~col:0 ~rows:0 ~cols:10 in
		assert (Rect.is_empty r));

	test "zero cols is empty" (fun () ->
		let r = Rect.make ~row:0 ~col:0 ~rows:10 ~cols:0 in
		assert (Rect.is_empty r));

	test "normal rectangle is not empty" (fun () ->
		let r = Rect.make ~row:0 ~col:0 ~rows:5 ~cols:5 in
		assert (not (Rect.is_empty r)));

	(* ------- contains ------- *)

	let r10 = Rect.make ~row:2 ~col:3 ~rows:4 ~cols:5 in

	test "contains interior point" (fun () ->
		assert (Rect.contains r10 ~row:3 ~col:5));

	test "contains top-left corner" (fun () ->
		assert (Rect.contains r10 ~row:2 ~col:3));

	test "contains just before bottom-right (half-open)" (fun () ->
		assert (Rect.contains r10 ~row:5 ~col:7));

	test "does not contain row past end" (fun () ->
		assert (not (Rect.contains r10 ~row:6 ~col:5)));

	test "does not contain col past end" (fun () ->
		assert (not (Rect.contains r10 ~row:3 ~col:8)));

	test "does not contain row before start" (fun () ->
		assert (not (Rect.contains r10 ~row:1 ~col:5)));

	test "does not contain col before start" (fun () ->
		assert (not (Rect.contains r10 ~row:3 ~col:2)));

	(* ------- split_h ------- *)

	let base = Rect.make ~row:0 ~col:0 ~rows:10 ~cols:20 in

	test "split_h at 0.5" (fun () ->
		let top, bot = Rect.split_h base ~ratio:0.5 in
		assert_rect ~expected:
			(Rect.make ~row:0 ~col:0 ~rows:5 ~cols:20)
			~actual:top;
		assert_rect ~expected:
			(Rect.make ~row:5 ~col:0 ~rows:5 ~cols:20)
			~actual:bot);

	test "split_h at 0.3 truncates" (fun () ->
		let top, bot = Rect.split_h base ~ratio:0.3 in
		assert (top.rows = 3 && bot.rows = 7);
		assert (top.row = 0 && bot.row = 3);
		assert (top.rows + bot.rows = base.rows));

	test "split_h at 0.0 top is empty" (fun () ->
		let top, bot = Rect.split_h base ~ratio:0.0 in
		assert (Rect.is_empty top);
		assert (bot.rows = base.rows));

	test "split_h at 1.0 bottom is empty" (fun () ->
		let top, bot = Rect.split_h base ~ratio:1.0 in
		assert (top.rows = base.rows);
		assert (Rect.is_empty bot));

	test "split_h clamps ratio > 1.0" (fun () ->
		let top, bot = Rect.split_h base ~ratio:2.0 in
		assert (top.rows = base.rows);
		assert (Rect.is_empty bot));

	test "split_h preserves offset" (fun () ->
		let r = Rect.make ~row:10 ~col:5 ~rows:8 ~cols:20 in
		let top, bot = Rect.split_h r ~ratio:0.5 in
		assert (top.row = 10 && bot.row = 14);
		assert (top.col = 5 && bot.col = 5));

	(* ------- split_v ------- *)

	test "split_v at 0.5" (fun () ->
		let left, right = Rect.split_v base ~ratio:0.5 in
		assert_rect ~expected:
			(Rect.make ~row:0 ~col:0 ~rows:10 ~cols:10)
			~actual:left;
		assert_rect ~expected:
			(Rect.make ~row:0 ~col:10 ~rows:10 ~cols:10)
			~actual:right);

	test "split_v conserves cols" (fun () ->
		let left, right = Rect.split_v base ~ratio:0.37 in
		assert (left.cols + right.cols = base.cols));

	test "split_v preserves offset" (fun () ->
		let r = Rect.make ~row:3 ~col:7 ~rows:5 ~cols:10 in
		let left, right = Rect.split_v r ~ratio:0.5 in
		assert (left.row = 3 && right.row = 3);
		assert (left.col = 7 && right.col = 12));

	(* ------- intersect ------- *)

	test "intersect with self" (fun () ->
		let r = Rect.make ~row:2 ~col:3 ~rows:4 ~cols:5 in
		match Rect.intersect r r with
		| None -> failwith "expected self-intersect to be non-empty"
		| Some i -> assert_rect ~expected:r ~actual:i);

	test "intersect disjoint returns None" (fun () ->
		let a = Rect.make ~row:0 ~col:0 ~rows:5 ~cols:5 in
		let b = Rect.make ~row:10 ~col:10 ~rows:5 ~cols:5 in
		assert (Rect.intersect a b = None));

	test "intersect edge-touching returns None" (fun () ->
		(* a spans rows [0,5); b starts at row 5 — half-open, no overlap *)
		let a = Rect.make ~row:0 ~col:0 ~rows:5 ~cols:5 in
		let b = Rect.make ~row:5 ~col:0 ~rows:5 ~cols:5 in
		assert (Rect.intersect a b = None));

	test "intersect partial overlap" (fun () ->
		let a = Rect.make ~row:0 ~col:0 ~rows:10 ~cols:10 in
		let b = Rect.make ~row:5 ~col:5 ~rows:10 ~cols:10 in
		match Rect.intersect a b with
		| None -> failwith "expected partial overlap"
		| Some i ->
				assert_rect ~expected:
					(Rect.make ~row:5 ~col:5 ~rows:5 ~cols:5)
					~actual:i);

	test "intersect one fully inside other" (fun () ->
		let outer = Rect.make ~row:0 ~col:0 ~rows:10 ~cols:10 in
		let inner = Rect.make ~row:2 ~col:3 ~rows:4 ~cols:5 in
		match Rect.intersect outer inner with
		| None -> failwith "expected inner to be the intersection"
		| Some i -> assert_rect ~expected:inner ~actual:i);

	(* ------- shrink ------- *)

	test "shrink with all insets zero is identity" (fun () ->
		let r = Rect.make ~row:1 ~col:2 ~rows:10 ~cols:20 in
		let s = Rect.shrink r ~top:0 ~bottom:0 ~left:0 ~right:0 in
		assert_rect ~expected:r ~actual:s);

	test "shrink by 1 on each side" (fun () ->
		let r = Rect.make ~row:0 ~col:0 ~rows:10 ~cols:20 in
		let s = Rect.shrink r ~top:1 ~bottom:1 ~left:1 ~right:1 in
		assert_rect ~expected:
			(Rect.make ~row:1 ~col:1 ~rows:8 ~cols:18)
			~actual:s);

	test "shrink negative insets clamp to 0" (fun () ->
		let r = Rect.make ~row:0 ~col:0 ~rows:10 ~cols:10 in
		let s = Rect.shrink r ~top:(-5) ~bottom:0 ~left:0 ~right:0 in
		assert_rect ~expected:r ~actual:s);

	test "shrink past dimension produces empty" (fun () ->
		let r = Rect.make ~row:0 ~col:0 ~rows:5 ~cols:5 in
		let s = Rect.shrink r ~top:3 ~bottom:3 ~left:0 ~right:0 in
		assert (Rect.is_empty s));

	(* ------- clip_write ------- *)

	let clip = Rect.make ~row:2 ~col:5 ~rows:4 ~cols:10 in

	test "clip_write fully inside" (fun () ->
		match Terminal.clip_write ~clip ~row:3 ~col:6 "hello" with
		| None -> failwith "expected hit"
		| Some (r, c, s) ->
				assert (r = 3 && c = 6 && s = "hello"));

	test "clip_write row above clip" (fun () ->
		assert (Terminal.clip_write ~clip ~row:1 ~col:6 "x" = None));

	test "clip_write row below clip" (fun () ->
		assert (Terminal.clip_write ~clip ~row:6 ~col:6 "x" = None));

	test "clip_write fully left of clip" (fun () ->
		assert (Terminal.clip_write ~clip ~row:3 ~col:0 "abc" = None));

	test "clip_write fully right of clip" (fun () ->
		assert (Terminal.clip_write ~clip ~row:3 ~col:20 "abc" = None));

	test "clip_write left-truncated" (fun () ->
		(* string at col=2, clip starts at col=5 → drop first 3 chars *)
		match Terminal.clip_write ~clip ~row:3 ~col:2 "abcdefgh" with
		| None -> failwith "expected partial hit"
		| Some (r, c, s) ->
				assert (r = 3);
				assert (c = 5);
				assert (s = "defgh"));

	test "clip_write right-truncated" (fun () ->
		(* string at col=12, clip ends at col=15 → keep first 3 chars *)
		match Terminal.clip_write ~clip ~row:3 ~col:12 "abcdefgh" with
		| None -> failwith "expected partial hit"
		| Some (r, c, s) ->
				assert (r = 3);
				assert (c = 12);
				assert (s = "abc"));

	test "clip_write both-truncated" (fun () ->
		(* string spans col 2..20; clip is col 5..15 → keep chars 3..12 = 10 chars *)
		match Terminal.clip_write ~clip ~row:3 ~col:2
			"aaaaaaaaaaaaaaaaaaa" (* 19 a's *) with
		| None -> failwith "expected partial hit"
		| Some (r, c, s) ->
				assert (r = 3);
				assert (c = 5);
				assert (String.length s = 10));

	test "clip_write empty clip returns None" (fun () ->
		let empty_clip = Rect.make ~row:0 ~col:0 ~rows:0 ~cols:0 in
		assert (Terminal.clip_write ~clip:empty_clip
			~row:0 ~col:0 "hi" = None));

	test "clip_write edge col at clip_end is None" (fun () ->
		(* clip_end_col = 15; col = 15 → no space *)
		assert (Terminal.clip_write ~clip ~row:3 ~col:15 "x" = None));

	test "clip_write last visible col fits 1 char" (fun () ->
		match Terminal.clip_write ~clip ~row:3 ~col:14 "abc" with
		| None -> failwith "expected one-char fit"
		| Some (_, c, s) ->
				assert (c = 14 && s = "a"));

	print_endline "test_rect done"
