(** Eio-based event loop for twig applications.

    Runs input → update → render in an Eio fiber.
    Input is read in a systhread so the Eio scheduler
    stays responsive for concurrent operations. *)

type 'state config = {
	on_input : Input.event -> 'state -> 'state;
	on_update : 'state -> 'state;
	render : 'state -> string;
	should_quit : 'state -> bool;
}

val run : config:'state config -> init:'state -> unit -> 'state
(** Run the event loop until [should_quit] returns true.
    Must be called from within an Eio fiber context
    (inside [Eio_main.run] or under an [Eio.Switch.t]). *)
