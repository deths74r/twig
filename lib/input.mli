type direction = Up | Down | Left | Right

type event =
	| Char of Uchar.t
	| Enter
	| Backspace
	| Tab
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
	| Ctrl of char
	| Eof
	| Unknown

val read : unit -> event
