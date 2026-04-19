let esc = "\x1b["

let hide_cursor buf =
	Buffer.add_string buf esc;
	Buffer.add_string buf "?25l"

let show_cursor buf =
	Buffer.add_string buf esc;
	Buffer.add_string buf "?25h"

let move_cursor buf row col =
	Buffer.add_string buf esc;
	Buffer.add_string buf (string_of_int (row + 1));
	Buffer.add_char buf ';';
	Buffer.add_string buf (string_of_int (col + 1));
	Buffer.add_char buf 'H'

let clear_line buf =
	Buffer.add_string buf esc;
	Buffer.add_string buf "K"

let reverse_video buf =
	Buffer.add_string buf esc;
	Buffer.add_string buf "7m"

let reset_attrs buf =
	Buffer.add_string buf esc;
	Buffer.add_string buf "m"

let tab_width = 8

let iter_clusters f s =
	Uuseg_string.fold_utf_8 `Grapheme_cluster (fun () c -> f c) () s

type style = Plain | Selected | Matched

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

let find_segment_index segments cursor_gi =
	let rec loop idx = function
		| [] -> max 0 (idx - 1)
		| [_] -> idx
		| seg :: rest ->
			if cursor_gi >= seg.start_gi && cursor_gi < seg.end_gi then idx
			else loop (idx + 1) rest
	in
	loop 0 segments

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

let is_bracket c =
	c = '(' || c = ')' || c = '[' || c = ']' || c = '{' || c = '}'

let is_opening c = c = '(' || c = '[' || c = '{'

let bracket_color depth =
	let n = Array.length Theme.bracket_colors in
	Theme.bracket_colors.((max 0 depth) mod n)

let emit_segment buf line spans (theme : Theme.t) (seg : segment)
		max_width sel_range match_ranges
		bracket_depth bracket_match doc_row =
	let col = ref 0 in
	let grapheme_idx = ref 0 in
	let stopped = ref false in
	let current = ref Plain in
	let in_sel idx =
		match sel_range with
		| None -> false
		| Some (s, e) -> idx >= s && idx < e
	in
	let in_match idx =
		List.exists (fun (s, e) -> idx >= s && idx < e) match_ranges
	in
	let desired_style idx =
		if in_sel idx then Selected
		else if in_match idx then Matched
		else Plain
	in
	let exit_style = function
		| Plain -> ()
		| Selected -> Buffer.add_string buf theme.selection_exit
		| Matched -> Buffer.add_string buf theme.match_exit
	in
	let enter_style style syntax_color =
		match style with
		| Plain -> Buffer.add_string buf syntax_color
		| Selected -> Buffer.add_string buf theme.selection_enter
		| Matched -> Buffer.add_string buf theme.match_enter
	in
	List.iter (fun (span : Syntax.span) ->
		if !stopped then ()
		else begin
			let text = String.sub line span.start span.length in
			let syntax_color = Theme.color_for theme span.kind in
			let span_color_emitted = ref false in
			iter_clusters (fun cluster ->
				if !stopped then ()
				else if !grapheme_idx >= seg.end_gi then
					stopped := true
				else if !grapheme_idx < seg.start_gi then
					incr grapheme_idx
				else begin
					if not !span_color_emitted && !current = Plain then begin
						Buffer.add_string buf syntax_color;
						span_color_emitted := true
					end;
					let want = desired_style !grapheme_idx in
					if want <> !current then begin
						exit_style !current;
						enter_style want syntax_color;
						current := want
					end;
					let is_bracket_cluster =
						String.length cluster = 1
						&& is_bracket cluster.[0]
						&& (span.kind = Text || span.kind = Keyword
							|| span.kind = Type_kw)
					in
					let is_match_pos =
						match bracket_match with
						| Some (p : Position.t) ->
							p.line = doc_row
							&& p.column = !grapheme_idx
						| None -> false
					in
					if is_bracket_cluster && !current = Plain then begin
						let ch = cluster.[0] in
						if is_match_pos then
							Buffer.add_string buf Theme.bracket_match
						else if is_opening ch then
							Buffer.add_string buf
								(bracket_color !bracket_depth)
						else
							Buffer.add_string buf
								(bracket_color (max 0 (!bracket_depth - 1)));
						Buffer.add_string buf cluster;
						Buffer.add_string buf theme.reset;
						Buffer.add_string buf syntax_color;
						if is_opening ch then incr bracket_depth
						else bracket_depth := max 0 (!bracket_depth - 1);
						col := !col + 1
					end else
					if cluster = "\t" then begin
						let next = ((!col / tab_width) + 1) * tab_width in
						let spaces = next - !col in
						let allowed = min spaces (max_width - !col) in
						for _ = 1 to allowed do Buffer.add_char buf ' ' done;
						col := !col + allowed;
						if !col >= max_width then stopped := true
					end else begin
						let w = Grapheme.display_width cluster in
						if !col + w > max_width then stopped := true
						else begin
							Buffer.add_string buf cluster;
							col := !col + w
						end
					end;
					incr grapheme_idx
				end
			) text
		end
	) spans;
	exit_style !current;
	Buffer.add_string buf theme.reset

let digits_of n =
	if n <= 0 then 1
	else
		let rec loop n acc = if n = 0 then acc else loop (n / 10) (acc + 1) in
		loop n 0

let gutter_width (state : State.t) (ui : Ui.t) =
	if ui.show_line_numbers then
		let n = Doc.line_count state.doc in
		digits_of (max 1 n) + 1
	else if ui.show_diff_markers then 1
	else 0

let find_all_in_line line query =
	if query = "" then []
	else begin
		let ll = String.length line in
		let ql = String.length query in
		if ql > ll then []
		else begin
			let rec loop i acc =
				if i > ll - ql then List.rev acc
				else if String.sub line i ql = query then
					loop (i + ql) (i :: acc)
				else
					loop (i + 1) acc
			in
			loop 0 []
		end
	end

let preview_line_of (state : State.t) =
	match state.mode with
	| Command_prompt cp -> cp.preview_line
	| _ -> None

let line_match_ranges (state : State.t) line_text =
	match state.mode with
	| Searching search when search.query <> "" ->
		let byte_offsets = find_all_in_line line_text search.query in
		let query_graphemes = Grapheme.count search.query in
		List.map (fun bo ->
			let gi = Grapheme.index_of_byte line_text bo in
			(gi, gi + query_graphemes)
		) byte_offsets
	| _ -> []

let closing_of = function
	| '(' -> ')' | '[' -> ']' | '{' -> '}' | c -> c

let opening_of = function
	| ')' -> '(' | ']' -> '[' | '}' -> '{' | c -> c

let gutter_mark_for (state : State.t) line =
	match Doc.get_mark line state.doc with
	| Doc.Unchanged -> `None
	| Doc.Added -> `Added
	| Doc.Modified -> `Modified

let compute_bracket_depth (state : State.t) (ui : Ui.t) lang =
	let depth = ref 0 in
	for i = 0 to ui.top_line - 1 do
		match Doc.get_line i state.doc with
		| None -> ()
		| Some line ->
			let spans, _ =
				Syntax.tokenize_line line Syntax.Normal lang
			in
			List.iter (fun (span : Syntax.span) ->
				if span.kind = Text || span.kind = Keyword
					|| span.kind = Type_kw
				then begin
					let text = String.sub line span.start span.length in
					String.iter (fun c ->
						if is_opening c then incr depth
						else if is_bracket c && not (is_opening c) then
							depth := max 0 (!depth - 1)
					) text
				end
			) spans
	done;
	!depth

let find_bracket_match (state : State.t) lang =
	match Doc.get_line state.cursor.line state.doc with
	| None -> None
	| Some line ->
		let byte = Grapheme.byte_of_index line state.cursor.column in
		if byte >= String.length line then None
		else begin
			let c = Char.chr (Char.code line.[byte]) in
			if not (is_bracket c) then None
			else if is_opening c then begin
				let target = closing_of c in
				let depth = ref 1 in
				let result = ref None in
				let start_col = state.cursor.column + 1 in
				let max_lines =
					min (Doc.line_count state.doc)
						(state.cursor.line + 5000)
				in
				let i = ref state.cursor.line in
				let stop = ref false in
				while !i < max_lines && not !stop do
					(match Doc.get_line !i state.doc with
					| None -> ()
					| Some l ->
						let spans, _ =
							Syntax.tokenize_line l Syntax.Normal lang
						in
						List.iter (fun (span : Syntax.span) ->
							if !stop then ()
							else if span.kind = Text || span.kind = Keyword
								|| span.kind = Type_kw
							then begin
								let text =
									String.sub l span.start span.length
								in
								let gi = ref 0 in
								iter_clusters (fun cluster ->
									if !stop then ()
									else begin
										let abs_gi =
											Grapheme.index_of_byte l
												(span.start
												+ Grapheme.byte_of_index
													text !gi)
										in
										let skip =
											!i = state.cursor.line
											&& abs_gi < start_col
										in
										if not skip
											&& String.length cluster = 1
										then begin
											let ch = cluster.[0] in
											if ch = c then incr depth
											else if ch = target then begin
												decr depth;
												if !depth = 0 then begin
													result := Some {
														Position.line = !i;
														column = abs_gi;
													};
													stop := true
												end
											end
										end;
										incr gi
									end
								) text
							end
						) spans);
					incr i
				done;
				!result
			end else begin
				let target = opening_of c in
				let depth = ref 1 in
				let result = ref None in
				let end_col = state.cursor.column in
				let min_line =
					max 0 (state.cursor.line - 5000)
				in
				let i = ref state.cursor.line in
				let stop = ref false in
				while !i >= min_line && not !stop do
					(match Doc.get_line !i state.doc with
					| None -> ()
					| Some l ->
						let spans, _ =
							Syntax.tokenize_line l Syntax.Normal lang
						in
						let rev_spans = List.rev spans in
						List.iter (fun (span : Syntax.span) ->
							if !stop then ()
							else if span.kind = Text || span.kind = Keyword
								|| span.kind = Type_kw
							then begin
								let text =
									String.sub l span.start span.length
								in
								let clusters = ref [] in
								let gi = ref 0 in
								iter_clusters (fun cluster ->
									let abs_gi =
										Grapheme.index_of_byte l
											(span.start
											+ Grapheme.byte_of_index
												text !gi)
									in
									clusters :=
										(abs_gi, cluster) :: !clusters;
									incr gi
								) text;
								List.iter (fun (abs_gi, cluster) ->
									if !stop then ()
									else begin
										let skip =
											!i = state.cursor.line
											&& abs_gi >= end_col
										in
										if not skip
											&& String.length cluster = 1
										then begin
											let ch = cluster.[0] in
											if ch = c then incr depth
											else if ch = target then begin
												decr depth;
												if !depth = 0 then begin
													result := Some {
														Position.line = !i;
														column = abs_gi;
													};
													stop := true
												end
											end
										end
									end
								) !clusters
							end
						) rev_spans);
					decr i
				done;
				!result
			end
		end

let line_selection_range (state : State.t) line =
	match state.mark with
	| None -> None
	| Some mark ->
		let (a, b) =
			if (mark.line < state.cursor.line)
				|| (mark.line = state.cursor.line
					&& mark.column <= state.cursor.column)
			then (mark, state.cursor)
			else (state.cursor, mark)
		in
		if line < a.line || line > b.line then None
		else
			let start_col = if line = a.line then a.column else 0 in
			let end_col = if line = b.line then b.column else max_int in
			Some (start_col, end_col)

let initial_syntax_state (state : State.t) (ui : Ui.t) lang =
	let rec scan i st =
		if i >= ui.top_line then st
		else
			let line = Option.value (Doc.get_line i state.doc) ~default:"" in
			let _, st' = Syntax.tokenize_line line st lang in
			scan (i + 1) st'
	in
	scan 0 Syntax.Normal

let draw_content buf (state : State.t) (ui : Ui.t) (theme : Theme.t) =
	let lang = Syntax.language_of_filename state.filename in
	let gutter = gutter_width state ui in
	let content_max = max 1 (ui.cols - gutter) in
	let wrap_limit = if ui.wrap then content_max else max_int / 2 in
	let ch = Ui.content_rows ui in
	let syntax_state = ref (initial_syntax_state state ui lang) in
	let screen_row = ref 0 in
	let doc_row = ref ui.top_line in
	let cursor_screen = ref None in
	let total_lines = Doc.line_count state.doc in
	let bracket_depth = ref (compute_bracket_depth state ui lang) in
	let bracket_match = find_bracket_match state lang in
	while !screen_row < ch do
		if !doc_row >= total_lines then begin
			move_cursor buf !screen_row 0;
			clear_line buf;
			if gutter > 0 then begin
				if ui.show_line_numbers then begin
					let num_width = max 1 (gutter - 1) in
					let tilde_field =
						Printf.sprintf "%*s" num_width "~"
					in
					Buffer.add_string buf "\x1b[90m";
					Buffer.add_string buf tilde_field;
					Buffer.add_string buf "\x1b[0m";
					for _ = 1 to gutter - num_width do
						Buffer.add_char buf ' '
					done
				end else
					Buffer.add_string buf (String.make gutter ' ')
			end else
				Buffer.add_char buf '~';
			incr screen_row;
			incr doc_row
		end else begin
			let line =
				Option.value (Doc.get_line !doc_row state.doc) ~default:""
			in
			let spans, next_state =
				Syntax.tokenize_line line !syntax_state lang
			in
			syntax_state := next_state;
			let segments = wrap_line line wrap_limit in
			let n_segs = List.length segments in
			let sel = line_selection_range state !doc_row in
			let matches = line_match_ranges state line in
			List.iteri (fun seg_idx (seg : segment) ->
				if !screen_row < ch then begin
					move_cursor buf !screen_row 0;
					clear_line buf;
					if gutter > 0 then begin
						if seg_idx = 0 then begin
							let mark = gutter_mark_for state !doc_row in
							if ui.show_line_numbers then begin
								let num_str =
									Printf.sprintf "%*d"
										(gutter - 1) (!doc_row + 1)
								in
								Buffer.add_string buf "\x1b[90m";
								Buffer.add_string buf num_str
							end;
							(match mark with
							| `Added when ui.show_diff_markers ->
								Buffer.add_string buf Theme.gutter_added;
								Buffer.add_char buf '+'
							| `Modified when ui.show_diff_markers ->
								Buffer.add_string buf Theme.gutter_modified;
								Buffer.add_char buf '~'
							| _ -> Buffer.add_char buf ' ');
							Buffer.add_string buf theme.reset
						end else
							Buffer.add_string buf (String.make gutter ' ')
					end;
					let is_preview =
						preview_line_of state = Some !doc_row
					in
					if is_preview then
						Buffer.add_string buf "\x1b[48;5;236m";
					emit_segment buf line spans theme seg content_max
						sel matches bracket_depth bracket_match
						!doc_row;
					if is_preview then
						Buffer.add_string buf "\x1b[49m";
					(if !doc_row = state.cursor.line then begin
						let c = state.cursor.column in
						let is_last = seg_idx = n_segs - 1 in
						let in_seg =
							c >= seg.start_gi
							&& (c < seg.end_gi || (is_last && c = seg.end_gi))
						in
						if in_seg then begin
							let col_in_seg =
								cursor_col_in_segment line seg c
							in
							cursor_screen :=
								Some (!screen_row, gutter + col_in_seg)
						end
					end);
					incr screen_row
				end
			) segments;
			incr doc_row
		end
	done;
	!cursor_screen

let draw_status_bar buf (state : State.t) (ui : Ui.t) =
	let row = ui.rows - 2 in
	move_cursor buf row 0;
	clear_line buf;
	reverse_video buf;
	let filename =
		match state.filename with
		| Some f -> Filename.basename f
		| None -> "[no name]"
	in
	let dirty_marker = if state.dirty then "*" else "" in
	let left = Printf.sprintf " %s%s " filename dirty_marker in
	let n = Doc.line_count state.doc in
	let position =
		if n = 0 || state.cursor.line = 0 then "Top"
		else if state.cursor.line >= n - 1 then "Bot"
		else Printf.sprintf "%d%%" ((state.cursor.line + 1) * 100 / n)
	in
	let right =
		Printf.sprintf " %d:%d | %s "
			(state.cursor.line + 1) (state.cursor.column + 1) position
	in
	Buffer.add_string buf left;
	let padding =
		ui.cols - Grapheme.display_width left - Grapheme.display_width right
	in
	for _ = 1 to padding do Buffer.add_char buf ' ' done;
	if padding >= 0 then Buffer.add_string buf right;
	reset_attrs buf

let draw_message_bar buf (state : State.t) (ui : Ui.t) =
	let row = ui.rows - 1 in
	move_cursor buf row 0;
	clear_line buf;
	match state.mode with
	| Searching search ->
		let prompt = Printf.sprintf "Search: %s" search.query in
		Buffer.add_string buf (Grapheme.truncate_to_width prompt ui.cols)
	| Opening_file of_state ->
		let prompt = Printf.sprintf "Open: %s" of_state.path in
		Buffer.add_string buf (Grapheme.truncate_to_width prompt ui.cols)
	| Command_chord ->
		let legend =
			"s:save S:saveas q:quit f:find r:replace g:goto \
			 t:theme w:wrap n:nums D:diff h:help :cmd"
		in
		Buffer.add_string buf "\x1b[90m";
		Buffer.add_string buf (Grapheme.truncate_to_width legend ui.cols);
		Buffer.add_string buf "\x1b[0m"
	| Command_prompt cp ->
		let prompt = Printf.sprintf ":%s" cp.input in
		Buffer.add_string buf (Grapheme.truncate_to_width prompt ui.cols)
	| Edit ->
		match state.message with
		| Some m ->
			Buffer.add_string buf (Grapheme.truncate_to_width m ui.cols)
		| None -> ()


let adjust_viewport (state : State.t) (ui : Ui.t) wrap_limit =
	let top = ref ui.top_line in
	if state.cursor.line < !top then top := state.cursor.line;
	let ch = Ui.content_rows ui in
	let cursor_visible top_line =
		let rows = ref 0 in
		let i = ref top_line in
		let visible = ref false in
		let n = Doc.line_count state.doc in
		let stop = ref false in
		while not !stop && !rows < ch && !i < n do
			let line =
				Option.value (Doc.get_line !i state.doc) ~default:""
			in
			let segs = wrap_line line wrap_limit in
			let cnt = List.length segs in
			if !i = state.cursor.line then begin
				let seg_idx = find_segment_index segs state.cursor.column in
				if !rows + seg_idx < ch then visible := true;
				stop := true
			end else begin
				rows := !rows + cnt;
				incr i
			end
		done;
		!visible
	in
	while !top < state.cursor.line && not (cursor_visible !top) do
		incr top
	done;
	{ ui with top_line = max 0 !top }

let vertical_move (state : State.t) (ui : Ui.t) direction =
	let gutter = gutter_width state ui in
	let content_max = max 1 (ui.cols - gutter) in
	let wrap_limit = if ui.wrap then content_max else max_int / 2 in
	let line_text =
		Option.value (Doc.get_line state.cursor.line state.doc) ~default:""
	in
	let segments = wrap_line line_text wrap_limit in
	let cur_seg_idx = find_segment_index segments state.cursor.column in
	let cur_seg = List.nth segments cur_seg_idx in
	let target_col =
		cursor_col_in_segment line_text cur_seg state.cursor.column
	in
	let n_segs = List.length segments in
	match direction with
	| `Up ->
		if cur_seg_idx > 0 then begin
			let prev_seg = List.nth segments (cur_seg_idx - 1) in
			let gi =
				grapheme_at_col_in_segment line_text prev_seg target_col
			in
			Some { Position.line = state.cursor.line; column = gi }
		end
		else if state.cursor.line > 0 then begin
			let prev_line_idx = state.cursor.line - 1 in
			let prev_line =
				Option.value
					(Doc.get_line prev_line_idx state.doc) ~default:""
			in
			let prev_segs = wrap_line prev_line wrap_limit in
			let last_seg =
				List.nth prev_segs (List.length prev_segs - 1)
			in
			let gi =
				grapheme_at_col_in_segment prev_line last_seg target_col
			in
			Some { line = prev_line_idx; column = gi }
		end
		else None
	| `Down ->
		if cur_seg_idx + 1 < n_segs then begin
			let next_seg = List.nth segments (cur_seg_idx + 1) in
			let gi =
				grapheme_at_col_in_segment line_text next_seg target_col
			in
			Some { Position.line = state.cursor.line; column = gi }
		end
		else if state.cursor.line + 1 < Doc.line_count state.doc then begin
			let next_line_idx = state.cursor.line + 1 in
			let next_line =
				Option.value
					(Doc.get_line next_line_idx state.doc) ~default:""
			in
			let next_segs = wrap_line next_line wrap_limit in
			let first_seg = List.hd next_segs in
			let gi =
				grapheme_at_col_in_segment next_line first_seg target_col
			in
			Some { line = next_line_idx; column = gi }
		end
		else None

let resolve_theme (state : State.t) =
	match Theme.by_name state.theme_name with
	| Some t -> t
	| None -> Theme.default

let frame ?(theme = Theme.default) (state : State.t) (ui : Ui.t) =
	let theme =
		if state.theme_name = "default" then theme
		else resolve_theme state
	in
	let gutter = gutter_width state ui in
	let content_max = max 1 (ui.cols - gutter) in
	let wrap_limit = if ui.wrap then content_max else max_int / 2 in
	let ui = adjust_viewport state ui wrap_limit in
	let buf = Buffer.create 4096 in
	hide_cursor buf;
	move_cursor buf 0 0;
	let cursor_screen = draw_content buf state ui theme in
	draw_status_bar buf state ui;
	draw_message_bar buf state ui;
	(match state.mode with
	| Opening_file of_state ->
		let row = ui.rows - 1 in
		let col = 6 + Grapheme.display_width of_state.path in
		move_cursor buf row col
	| Command_prompt cp ->
		let row = ui.rows - 1 in
		let col = 1 + Grapheme.display_width cp.input in
		move_cursor buf row col
	| Command_chord ->
		let row = ui.rows - 1 in
		move_cursor buf row 0
	| _ ->
		(match cursor_screen with
		| Some (r, c) -> move_cursor buf r c
		| None -> move_cursor buf 0 gutter));
	show_cursor buf;
	(ui, Buffer.contents buf)
