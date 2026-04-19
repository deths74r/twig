type direction = Up | Down | Left | Right

type event =
	| Char of Uchar.t
	| Enter
	| Backspace
	| Tab
	| Shift_tab
	| Escape
	| Arrow of direction
	| Shift_arrow of direction
	| Home
	| End
	| Page_up
	| Page_down
	| Delete
	| Word_left
	| Word_right
	| Doc_home
	| Doc_end
	| Shift_space
	| Alt of char
	| Ctrl of char
	| Eof
	| Resize
	| Unknown

val read : unit -> event
(** Block for the next input event. Returns early as [Unknown]
    if [wake ()] is called from another fiber, within the next
    100ms (the internal select timeout). *)

val wake : unit -> unit
(** Flip the wake flag so the next [read] (or the one in progress
    at its next select-timeout iteration) returns [Unknown]
    instead of continuing to block. Used to drive the TUI render
    loop on background events (Event_bus publishes). Thread-safe:
    multiple wakes before a read is fine — the flag is sticky
    until [read] consumes it. *)
