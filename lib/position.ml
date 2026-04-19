type t = {
	line : int;
	column : int;
}

let origin = { line = 0; column = 0 }

let equal a b = a.line = b.line && a.column = b.column

let compare a b =
	let c = compare a.line b.line in
	if c <> 0 then c else compare a.column b.column
