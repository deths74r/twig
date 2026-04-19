(** Viewport primitives — line wrapping, segment math.

    Pure functions for computing how text maps to screen
    coordinates. No dependency on editor state, themes, or
    syntax. Used by the render layer and by applications
    that need wrap-aware cursor positioning. *)

let tab_width = 8

let iter_clusters f s =
	Uuseg_string.fold_utf_8 `Grapheme_cluster (fun () c -> f c) () s

type segment = {
	start_gi : int;
	end_gi : int;
}

let measure_cluster col cluster =
	if cluster = "\t" then
		let next = ((col / tab_width) + 1) * tab_width in
		next - col
	else
		Grapheme.display_width cluster

(** Split a line into wrap segments, each fitting within
    [max_width] display columns. Returns a single segment
    covering the whole line if [max_width <= 0] or the
    line is empty. *)
let wrap_line line max_width =
	if max_width <= 0 || line = "" then
		[{ start_gi = 0; end_gi = Grapheme.count line }]
	else begin
		let segments = ref [] in
		let seg_start = ref 0 in
		let grapheme_idx = ref 0 in
		let col = ref 0 in
		iter_clusters (fun cluster ->
			let w = measure_cluster !col cluster in
			if !col + w > max_width && !col > 0 then begin
				segments :=
					{ start_gi = !seg_start; end_gi = !grapheme_idx }
					:: !segments;
				seg_start := !grapheme_idx;
				col := 0
			end;
			let w_actual = measure_cluster !col cluster in
			col := !col + w_actual;
			incr grapheme_idx
		) line;
		segments :=
			{ start_gi = !seg_start; end_gi = !grapheme_idx } :: !segments;
		List.rev !segments
	end

(** Find which segment index a grapheme position falls in. *)
let find_segment_index segments cursor_gi =
	let rec loop idx = function
		| [] -> max 0 (idx - 1)
		| [_] -> idx
		| seg :: rest ->
			if cursor_gi >= seg.start_gi && cursor_gi < seg.end_gi then idx
			else loop (idx + 1) rest
	in
	loop 0 segments

(** Compute the display column of a grapheme within a segment. *)
let cursor_col_in_segment line (seg : segment) cursor_gi =
	let col = ref 0 in
	let grapheme_idx = ref 0 in
	let done_ = ref false in
	iter_clusters (fun cluster ->
		if !done_ then ()
		else if !grapheme_idx < seg.start_gi then
			incr grapheme_idx
		else if !grapheme_idx >= cursor_gi then
			done_ := true
		else begin
			let w = measure_cluster !col cluster in
			col := !col + w;
			incr grapheme_idx
		end
	) line;
	!col

(** Reverse mapping: display column → grapheme index within
    a segment. Used for vertical cursor movement across
    wrapped lines. *)
let grapheme_at_col_in_segment line (seg : segment) target_col =
	let col = ref 0 in
	let grapheme_idx = ref 0 in
	let result = ref seg.end_gi in
	let found = ref false in
	iter_clusters (fun cluster ->
		if !found then ()
		else if !grapheme_idx < seg.start_gi then
			incr grapheme_idx
		else if !grapheme_idx >= seg.end_gi then begin
			result := seg.end_gi;
			found := true
		end else if !col >= target_col then begin
			result := !grapheme_idx;
			found := true
		end else begin
			let w = measure_cluster !col cluster in
			col := !col + w;
			incr grapheme_idx
		end
	) line;
	if not !found then result := !grapheme_idx;
	!result
