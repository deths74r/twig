val frame : ?theme:Theme.t -> State.t -> Ui.t -> Ui.t * string

val vertical_move :
	State.t -> Ui.t -> [`Up | `Down] -> Position.t option
