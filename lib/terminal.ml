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

let write s =
	let len = String.length s in
	let written = ref 0 in
	while !written < len do
		let n = Unix.write_substring Unix.stdout s !written (len - !written) in
		written := !written + n
	done

(* SIGWINCH is signal 28 on Linux and BSD. OCaml's Sys module doesn't
   expose it by name, so the number is hardcoded. *)
let sigwinch = 28

let resize_received = ref false

let install_resize_handler () =
	Sys.set_signal sigwinch
		(Sys.Signal_handle (fun _ -> resize_received := true))

let resize_flag () = !resize_received

let clear_resize_flag () = resize_received := false
