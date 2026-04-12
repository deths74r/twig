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
