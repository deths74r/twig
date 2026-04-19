type saved_state = Unix.terminal_io

let enter_raw_mode () =
	let original = Unix.tcgetattr Unix.stdin in
	let raw = {
		original with
		c_brkint = false;
		c_icrnl = false;
		c_inpck = false;
		c_istrip = false;
		c_ixon = false;
		c_opost = false;
		c_csize = 8;
		c_echo = false;
		c_icanon = false;
		c_isig = false;
		c_vmin = 1;
		c_vtime = 0;
	} in
	Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH raw;
	original

let restore saved =
	Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH saved

let get_size () =
	try
		let ic = Unix.open_process_in "stty size </dev/tty 2>/dev/null" in
		let line =
			try input_line ic
			with End_of_file -> ""
		in
		let _ = Unix.close_process_in ic in
		if line = "" then (24, 80)
		else Scanf.sscanf line "%d %d" (fun r c -> (r, c))
	with _ -> (24, 80)

let enable_kitty_keyboard () =
	let s = "\x1b[>25u" in
	let _ = Unix.write_substring Unix.stdout s 0 (String.length s) in
	()

let disable_kitty_keyboard () =
	let s = "\x1b[<u" in
	let _ = Unix.write_substring Unix.stdout s 0 (String.length s) in
	()

let default_writer : string -> unit = fun s ->
	let len = String.length s in
	let written = ref 0 in
	while !written < len do
		let n = Unix.write_substring Unix.stdout s !written (len - !written) in
		written := !written + n
	done

let writer : (string -> unit) ref = ref default_writer

let write s = !writer s

let with_writer w f =
	let prior = !writer in
	writer := w;
	Fun.protect ~finally:(fun () -> writer := prior) f

let with_capture f =
	let buf = Buffer.create 256 in
	let result = with_writer (Buffer.add_string buf) f in
	(Buffer.contents buf, result)

(* SIGWINCH is signal 28 on Linux and BSD. OCaml's Sys module doesn't
   expose it by name, so the number is hardcoded. *)
let sigwinch = 28

let resize_received = ref false

let install_resize_handler () =
	Sys.set_signal sigwinch
		(Sys.Signal_handle (fun _ -> resize_received := true))

let resize_flag () = !resize_received

let clear_resize_flag () = resize_received := false

(* ------------------------------------------------------------------ *)
(* Region-scoped positional writes                                    *)
(* ------------------------------------------------------------------ *)

let current_clip : Rect.t option ref = ref None

let move ~row ~col =
	(* CSI uses 1-based coordinates *)
	let s = Printf.sprintf "\x1b[%d;%dH" (row + 1) (col + 1) in
	write s

let clip_write ~clip ~row ~col s =
	if Rect.is_empty clip then None
	else if row < clip.row || row >= clip.row + clip.rows then None
	else
		let len = String.length s in
		let clip_end_col = clip.col + clip.cols in
		if col >= clip_end_col then None
		else if col + len <= clip.col then None
		else begin
			(* Left truncate *)
			let start_offset =
				if col < clip.col then clip.col - col else 0
			in
			let effective_col = col + start_offset in
			let remaining = len - start_offset in
			(* Right truncate *)
			let max_len = clip_end_col - effective_col in
			let final_len = if remaining > max_len then max_len else remaining in
			if final_len <= 0 then None
			else
				Some (row, effective_col,
				      String.sub s start_offset final_len)
		end

let write_at ~row ~col s =
	match !current_clip with
	| None ->
			move ~row ~col;
			write s
	| Some clip ->
			begin match clip_write ~clip ~row ~col s with
			| None -> ()
			| Some (r, c, s') ->
					move ~row:r ~col:c;
					write s'
			end

let clear_rect (r : Rect.t) =
	if Rect.is_empty r then ()
	else
		let effective =
			match !current_clip with
			| None -> Some r
			| Some c -> Rect.intersect r c
		in
		match effective with
		| None -> ()
		| Some r ->
				let blank = String.make r.cols ' ' in
				for i = 0 to r.rows - 1 do
					move ~row:(r.row + i) ~col:r.col;
					write blank
				done

let with_clip r f =
	let prior = !current_clip in
	let effective =
		match prior with
		| None -> Some r
		| Some c -> Rect.intersect r c
	in
	current_clip := effective;
	Fun.protect ~finally:(fun () -> current_clip := prior) f
