type measure = {
	lines : int;
	bytes : int;
}

type t =
	| Empty
	| Node of {
		line : string;
		left : t;
		right : t;
		height : int;
		size : int;
		byte_sum : int;
	}

let height = function
	| Empty -> 0
	| Node n -> n.height

let size_of = function
	| Empty -> 0
	| Node n -> n.size

let byte_sum = function
	| Empty -> 0
	| Node n -> n.byte_sum

let make_raw line left right =
	Node {
		line;
		left;
		right;
		height = 1 + max (height left) (height right);
		size = 1 + size_of left + size_of right;
		byte_sum = String.length line + byte_sum left + byte_sum right;
	}

let rotate_left t =
	match t with
	| Node {
		line = x; left = a;
		right = Node { line = y; left = b; right = c; _ };
		_
	} ->
		make_raw y (make_raw x a b) c
	| _ -> t

let rotate_right t =
	match t with
	| Node {
		line = y;
		left = Node { line = x; left = a; right = b; _ };
		right = c; _
	} ->
		make_raw x a (make_raw y b c)
	| _ -> t

let balance_factor = function
	| Empty -> 0
	| Node n -> height n.left - height n.right

let balance t =
	let bf = balance_factor t in
	if bf > 1 then
		match t with
		| Node n when balance_factor n.left < 0 ->
			rotate_right (make_raw n.line (rotate_left n.left) n.right)
		| _ -> rotate_right t
	else if bf < -1 then
		match t with
		| Node n when balance_factor n.right > 0 ->
			rotate_left (make_raw n.line n.left (rotate_right n.right))
		| _ -> rotate_left t
	else t

let make line left right = balance (make_raw line left right)

let empty = Empty

let singleton line = make line Empty Empty

let line_count = size_of

let measure t =
	let lines = size_of t in
	let bytes = byte_sum t + max 0 (lines - 1) in
	{ lines; bytes }

let rec get_line i t =
	match t with
	| Empty -> None
	| Node { left; line; right; _ } ->
		let ls = size_of left in
		if i < ls then get_line i left
		else if i = ls then Some line
		else get_line (i - ls - 1) right

let rec insert_line i new_line t =
	match t with
	| Empty ->
		if i <= 0 then singleton new_line else Empty
	| Node { left; line; right; _ } ->
		let ls = size_of left in
		if i <= ls then
			make line (insert_line i new_line left) right
		else
			make line left (insert_line (i - ls - 1) new_line right)

let rec extract_min = function
	| Empty -> None
	| Node { left = Empty; line; right; _ } -> Some (line, right)
	| Node { left; line; right; _ } ->
		(match extract_min left with
		| None -> None
		| Some (m, new_left) -> Some (m, make line new_left right))

let concat_siblings a b =
	match a, b with
	| Empty, _ -> b
	| _, Empty -> a
	| _ ->
		match extract_min b with
		| None -> a
		| Some (m, b') -> make m a b'

let rec delete_line i t =
	match t with
	| Empty -> Empty
	| Node { left; line; right; _ } ->
		let ls = size_of left in
		if i < ls then
			make line (delete_line i left) right
		else if i = ls then
			concat_siblings left right
		else
			make line left (delete_line (i - ls - 1) right)

let rec replace_line i new_line t =
	match t with
	| Empty -> Empty
	| Node { left; line; right; _ } ->
		let ls = size_of left in
		if i < ls then
			make line (replace_line i new_line left) right
		else if i = ls then
			make new_line left right
		else
			make line left (replace_line (i - ls - 1) new_line right)

let of_string s =
	if s = "" then Empty
	else begin
		let lines = String.split_on_char '\n' s in
		List.fold_left
			(fun acc l -> insert_line (size_of acc) l acc)
			Empty lines
	end

let of_channel ic =
	let acc = ref Empty in
	(try
		while true do
			acc := insert_line (size_of !acc) (input_line ic) !acc
		done
	with End_of_file -> ());
	!acc

let to_string t =
	let buf = Buffer.create 256 in
	let first = ref true in
	let rec walk = function
		| Empty -> ()
		| Node { left; line; right; _ } ->
			walk left;
			if !first then first := false
			else Buffer.add_char buf '\n';
			Buffer.add_string buf line;
			walk right
	in
	walk t;
	Buffer.contents buf

let insert_at ~line ~column text t =
	let n = line_count t in
	if line < 0 || line > n then t
	else begin
		let existing =
			if line = n then ""
			else Option.value (get_line line t) ~default:""
		in
		let byte_col = Grapheme.byte_of_index existing column in
		let prefix = String.sub existing 0 byte_col in
		let suffix =
			String.sub existing byte_col (String.length existing - byte_col)
		in
		let combined = prefix ^ text ^ suffix in
		let new_lines = String.split_on_char '\n' combined in
		match new_lines with
		| [] -> t
		| [single] ->
			if line = n then insert_line line single t
			else replace_line line single t
		| first :: rest ->
			let t =
				if line = n then insert_line line first t
				else replace_line line first t
			in
			let rec insert_rest i lines t =
				match lines with
				| [] -> t
				| l :: rest -> insert_rest (i + 1) rest (insert_line i l t)
			in
			insert_rest (line + 1) rest t
	end

let delete_range ~start_line ~start_col ~end_line ~end_col t =
	let n = line_count t in
	if start_line < 0 || start_line >= n then t
	else if start_line > end_line then t
	else if start_line = end_line && start_col >= end_col then t
	else begin
		let start_text =
			Option.value (get_line start_line t) ~default:""
		in
		let end_text =
			if end_line >= n then ""
			else Option.value (get_line end_line t) ~default:""
		in
		let start_byte = Grapheme.byte_of_index start_text start_col in
		let end_byte = Grapheme.byte_of_index end_text end_col in
		let prefix = String.sub start_text 0 start_byte in
		let suffix =
			if end_line >= n then ""
			else
				String.sub end_text end_byte
					(String.length end_text - end_byte)
		in
		let merged = prefix ^ suffix in
		let t = replace_line start_line merged t in
		let lines_to_drop = min end_line (line_count t - 1) - start_line in
		let rec drop k t =
			if k <= 0 then t
			else drop (k - 1) (delete_line (start_line + 1) t)
		in
		drop lines_to_drop t
	end

let find_substring_from s pat start =
	let sl = String.length s in
	let pl = String.length pat in
	if pl = 0 then if start <= sl then Some start else None
	else begin
		let limit = sl - pl in
		let rec loop i =
			if i > limit then None
			else if String.sub s i pl = pat then Some i
			else loop (i + 1)
		in
		loop start
	end

let find_forward t ~(from : Position.t) ~query =
	if query = "" then None
	else
		let n = line_count t in
		if n = 0 then None
		else
			let find_on_line line start_col =
				match get_line line t with
				| None -> None
				| Some text ->
					let start_byte = Grapheme.byte_of_index text start_col in
					(match find_substring_from text query start_byte with
					| Some b ->
						let column = Grapheme.index_of_byte text b in
						Some { Position.line; column }
					| None -> None)
			in
			let rec walk line start_col =
				if line >= n then None
				else match find_on_line line start_col with
					| Some p -> Some p
					| None -> walk (line + 1) 0
			in
			match walk from.line from.column with
			| Some p -> Some p
			| None ->
				let rec walk_wrapped line =
					if line > from.line then None
					else match find_on_line line 0 with
						| Some (p : Position.t) ->
							if p.line < from.line
								|| (p.line = from.line && p.column < from.column)
							then Some p
							else None
						| None -> walk_wrapped (line + 1)
				in
				walk_wrapped 0

let extract_range t ~(start : Position.t) ~(stop : Position.t) =
	let (a, b) =
		if (start.line < stop.line)
			|| (start.line = stop.line && start.column <= stop.column)
		then (start, stop)
		else (stop, start)
	in
	if Position.equal a b then ""
	else if a.line = b.line then
		match get_line a.line t with
		| None -> ""
		| Some line ->
			let sb = Grapheme.byte_of_index line a.column in
			let eb = Grapheme.byte_of_index line b.column in
			String.sub line sb (eb - sb)
	else begin
		let buf = Buffer.create 256 in
		(match get_line a.line t with
		| Some line ->
			let sb = Grapheme.byte_of_index line a.column in
			Buffer.add_string buf
				(String.sub line sb (String.length line - sb))
		| None -> ());
		Buffer.add_char buf '\n';
		for i = a.line + 1 to b.line - 1 do
			(match get_line i t with
			| Some line -> Buffer.add_string buf line
			| None -> ());
			Buffer.add_char buf '\n'
		done;
		(match get_line b.line t with
		| Some line ->
			let eb = Grapheme.byte_of_index line b.column in
			Buffer.add_string buf (String.sub line 0 eb)
		| None -> ());
		Buffer.contents buf
	end

let advance t (p : Position.t) : Position.t =
	match get_line p.line t with
	| None -> p
	| Some text ->
		let len = Grapheme.count text in
		if p.column < len then { p with column = p.column + 1 }
		else if p.line + 1 < line_count t then
			{ line = p.line + 1; column = 0 }
		else p
