type t = {
	line : int;
	column : int;
}

let origin = { line = 0; column = 0 }

let equal a b = a.line = b.line && a.column = b.column
