type token =
	| Text
	| Keyword
	| Type_kw
	| String_lit
	| Char_lit
	| Number
	| Comment
	| Preproc

type state =
	| Normal
	| In_block_comment of int

type span = {
	start : int;
	length : int;
	kind : token;
}

type language =
	| Plain
	| C
	| Ocaml

let language_of_filename = function
	| None -> Plain
	| Some path ->
		match String.lowercase_ascii (Filename.extension path) with
		| ".c" | ".h" | ".cc" | ".cpp" | ".cxx" | ".hpp" | ".hxx" -> C
		| ".ml" | ".mli" | ".mll" | ".mly" -> Ocaml
		| _ -> Plain

let c_keywords = [
	"if"; "else"; "while"; "for"; "do"; "switch"; "case"; "default";
	"return"; "break"; "continue"; "goto"; "sizeof"; "typeof";
]

let c_type_keywords = [
	"int"; "char"; "short"; "long"; "float"; "double"; "void";
	"signed"; "unsigned"; "const"; "static"; "extern"; "struct"; "union";
	"enum"; "typedef"; "auto"; "register"; "volatile"; "inline"; "restrict";
	"bool"; "_Bool"; "size_t"; "ssize_t"; "ptrdiff_t";
	"uint8_t"; "uint16_t"; "uint32_t"; "uint64_t";
	"int8_t"; "int16_t"; "int32_t"; "int64_t";
	"nullptr"; "true"; "false"; "NULL";
]

let ocaml_keywords = [
	"let"; "in"; "match"; "with"; "fun"; "function"; "if"; "then"; "else";
	"rec"; "and"; "or"; "as"; "begin"; "end"; "while"; "for"; "do"; "done";
	"to"; "downto"; "try"; "raise"; "module"; "struct"; "sig"; "open";
	"include"; "of"; "when"; "exception"; "val"; "external"; "mutable";
	"private"; "virtual"; "class"; "new"; "inherit"; "object"; "method";
	"initializer"; "constraint"; "lazy"; "assert"; "nonrec"; "type";
]

let ocaml_type_keywords = [
	"int"; "char"; "string"; "bool"; "float"; "unit"; "bytes"; "exn";
	"list"; "array"; "option"; "ref"; "true"; "false";
]

let is_word_start c =
	(c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

let is_word_char c =
	is_word_start c || (c >= '0' && c <= '9')

let is_digit c = c >= '0' && c <= '9'

let find_c_comment_end s start =
	let len = String.length s in
	let rec loop k =
		if k + 1 >= len then (len, false)
		else if s.[k] = '*' && s.[k + 1] = '/' then (k + 2, true)
		else loop (k + 1)
	in
	loop start

let tokenize_c s initial_state =
	let len = String.length s in
	let spans = ref [] in
	let emit start length kind =
		if length > 0 then spans := { start; length; kind } :: !spans
	in
	let i = ref 0 in
	let state = ref initial_state in
	while !i < len do
		match !state with
		| In_block_comment _ ->
			let start = !i in
			let stop, closed = find_c_comment_end s !i in
			emit start (stop - start) Comment;
			if closed then state := Normal;
			i := stop
		| Normal ->
			let c = s.[!i] in
			if c = '/' && !i + 1 < len && s.[!i + 1] = '/' then begin
				emit !i (len - !i) Comment;
				i := len
			end
			else if c = '/' && !i + 1 < len && s.[!i + 1] = '*' then begin
				let start = !i in
				let stop, closed = find_c_comment_end s (!i + 2) in
				emit start (stop - start) Comment;
				state := if closed then Normal else In_block_comment 1;
				i := stop
			end
			else if c = '"' then begin
				let start = !i in
				let j = ref (!i + 1) in
				while !j < len && s.[!j] <> '"' do
					if s.[!j] = '\\' && !j + 1 < len then j := !j + 2
					else incr j
				done;
				if !j < len then incr j;
				emit start (!j - start) String_lit;
				i := !j
			end
			else if c = '\'' then begin
				let start = !i in
				let j = ref (!i + 1) in
				while !j < len && s.[!j] <> '\'' do
					if s.[!j] = '\\' && !j + 1 < len then j := !j + 2
					else incr j
				done;
				if !j < len then incr j;
				emit start (!j - start) Char_lit;
				i := !j
			end
			else if c = '#' && !i = 0 then begin
				emit !i (len - !i) Preproc;
				i := len
			end
			else if is_digit c then begin
				let start = !i in
				let j = ref (!i + 1) in
				while !j < len
					&& (is_word_char s.[!j] || s.[!j] = '.')
				do incr j done;
				emit start (!j - start) Number;
				i := !j
			end
			else if is_word_start c then begin
				let start = !i in
				let j = ref (!i + 1) in
				while !j < len && is_word_char s.[!j] do incr j done;
				let word = String.sub s start (!j - start) in
				let kind =
					if List.mem word c_keywords then Keyword
					else if List.mem word c_type_keywords then Type_kw
					else Text
				in
				emit start (!j - start) kind;
				i := !j
			end
			else begin
				emit !i 1 Text;
				incr i
			end
	done;
	(List.rev !spans, !state)

let find_ocaml_comment_end s start depth =
	let len = String.length s in
	let d = ref depth in
	let i = ref start in
	while !d > 0 && !i < len do
		if !i + 1 < len && s.[!i] = '(' && s.[!i + 1] = '*' then begin
			incr d;
			i := !i + 2
		end else if !i + 1 < len && s.[!i] = '*' && s.[!i + 1] = ')' then begin
			decr d;
			i := !i + 2
		end else
			incr i
	done;
	(!i, !d)

let tokenize_ocaml s initial_state =
	let len = String.length s in
	let spans = ref [] in
	let emit start length kind =
		if length > 0 then spans := { start; length; kind } :: !spans
	in
	let i = ref 0 in
	let state = ref initial_state in
	while !i < len do
		match !state with
		| In_block_comment d ->
			let start = !i in
			let stop, new_d = find_ocaml_comment_end s !i d in
			emit start (stop - start) Comment;
			state := if new_d = 0 then Normal else In_block_comment new_d;
			i := stop
		| Normal ->
			let c = s.[!i] in
			if c = '(' && !i + 1 < len && s.[!i + 1] = '*' then begin
				let start = !i in
				let stop, new_d = find_ocaml_comment_end s (!i + 2) 1 in
				emit start (stop - start) Comment;
				state := if new_d = 0 then Normal else In_block_comment new_d;
				i := stop
			end
			else if c = '"' then begin
				let start = !i in
				let j = ref (!i + 1) in
				while !j < len && s.[!j] <> '"' do
					if s.[!j] = '\\' && !j + 1 < len then j := !j + 2
					else incr j
				done;
				if !j < len then incr j;
				emit start (!j - start) String_lit;
				i := !j
			end
			else if is_digit c then begin
				let start = !i in
				let j = ref (!i + 1) in
				while !j < len
					&& (is_word_char s.[!j] || s.[!j] = '.')
				do incr j done;
				emit start (!j - start) Number;
				i := !j
			end
			else if is_word_start c then begin
				let start = !i in
				let j = ref (!i + 1) in
				while !j < len && (is_word_char s.[!j] || s.[!j] = '\'')
				do incr j done;
				let word = String.sub s start (!j - start) in
				let kind =
					if List.mem word ocaml_keywords then Keyword
					else if List.mem word ocaml_type_keywords then Type_kw
					else Text
				in
				emit start (!j - start) kind;
				i := !j
			end
			else begin
				emit !i 1 Text;
				incr i
			end
	done;
	(List.rev !spans, !state)

let tokenize_plain s _ =
	if String.length s = 0 then ([], Normal)
	else ([{ start = 0; length = String.length s; kind = Text }], Normal)

let tokenize_line s state lang =
	match lang with
	| Plain -> tokenize_plain s state
	| C -> tokenize_c s state
	| Ocaml -> tokenize_ocaml s state
