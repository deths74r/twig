type saved_state

val enter_raw_mode : unit -> saved_state

val restore : saved_state -> unit

val enable_kitty_keyboard : unit -> unit

val disable_kitty_keyboard : unit -> unit

val get_size : unit -> int * int

val write : string -> unit

val install_resize_handler : unit -> unit

val resize_flag : unit -> bool

val clear_resize_flag : unit -> unit
