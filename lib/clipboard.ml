let b64_alphabet =
	"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let base64_encode s =
	let len = String.length s in
	let out = Buffer.create (((len + 2) / 3) * 4) in
	let i = ref 0 in
	while !i < len do
		let b0 = Char.code s.[!i] in
		let b1 = if !i + 1 < len then Char.code s.[!i + 1] else 0 in
		let b2 = if !i + 2 < len then Char.code s.[!i + 2] else 0 in
		let c0 = b0 lsr 2 in
		let c1 = ((b0 land 0x3) lsl 4) lor (b1 lsr 4) in
		let c2 = ((b1 land 0xF) lsl 2) lor (b2 lsr 6) in
		let c3 = b2 land 0x3F in
		Buffer.add_char out b64_alphabet.[c0];
		Buffer.add_char out b64_alphabet.[c1];
		if !i + 1 < len then Buffer.add_char out b64_alphabet.[c2]
		else Buffer.add_char out '=';
		if !i + 2 < len then Buffer.add_char out b64_alphabet.[c3]
		else Buffer.add_char out '=';
		i := !i + 3
	done;
	Buffer.contents out

let osc52_set s =
	Printf.sprintf "\x1b]52;c;%s\x07" (base64_encode s)
