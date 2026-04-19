(** Eio-based event loop for twig applications.

    Runs the input → update → render cycle as an Eio fiber.
    [Input.read] is dispatched to a systhread so the Eio
    scheduler stays responsive while waiting for keyboard
    input. Between input events, the loop drains an
    [Eio.Stream.t] for updates pushed by concurrent fibers
    (agent completions, timer events, network responses).

    Applications provide three callbacks:
    - [on_input] — handle a keyboard event
    - [on_update] — handle an external update
    - [render] — produce output for the terminal

    The loop runs until [should_quit] returns true. *)

type 'state config = {
	on_input : Input.event -> 'state -> 'state;
		(** Handle a keyboard event. *)
	on_update : 'state -> 'state;
		(** Handle external updates (called after draining
		    the update stream). Override for app-specific
		    update processing. Default: identity. *)
	render : 'state -> string;
		(** Produce a terminal frame string from the current
		    state. Called once per loop iteration. *)
	should_quit : 'state -> bool;
		(** Return true to exit the loop. *)
}

let run ~(config : 'state config) ~(init : 'state) () : 'state =
	let state = ref init in
	while not (config.should_quit !state) do
		(* Check for terminal resize *)
		if Terminal.resize_flag () then begin
			Terminal.clear_resize_flag ();
			state := config.on_input Input.Resize !state
		end;
		(* Render current state *)
		let frame = config.render !state in
		Terminal.write frame;
		(* Read input in a systhread so Eio can schedule
		   other fibers while we wait for a keypress.
		   Input.read uses Unix.select with a 0.1s timeout
		   internally — that 0.1s is when the systhread
		   yields back if no key arrives. *)
		let event =
			Eio_unix.run_in_systhread (fun () -> Input.read ())
		in
		(* Apply input if we got one (Input.read loops
		   internally on timeout, so it always returns
		   a real event eventually — but resize can
		   interrupt it) *)
		state := config.on_input event !state;
		(* Process any external updates *)
		state := config.on_update !state
	done;
	!state
