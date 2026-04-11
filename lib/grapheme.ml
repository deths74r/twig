let cluster_starts s =
	let clusters = ref [] in
	Uuseg_string.fold_utf_8 `Grapheme_cluster
		(fun () c -> clusters := c :: !clusters)
		() s;
	let clusters = List.rev !clusters in
	let n = List.length clusters in
	let arr = Array.make (n + 1) 0 in
	let pos = ref 0 in
	List.iteri (fun i c ->
		arr.(i) <- !pos;
		pos := !pos + String.length c
	) clusters;
	arr.(n) <- !pos;
	arr

let count s = Array.length (cluster_starts s) - 1

let byte_of_index s i =
	let starts = cluster_starts s in
	let n = Array.length starts - 1 in
	if i <= 0 then 0
	else if i >= n then starts.(n)
	else starts.(i)

let index_of_byte s b =
	let starts = cluster_starts s in
	let lo = ref 0 in
	let hi = ref (Array.length starts - 1) in
	while !lo < !hi do
		let mid = (!lo + !hi + 1) / 2 in
		if starts.(mid) <= b then lo := mid
		else hi := mid - 1
	done;
	!lo

let uchar_width u =
	let w = Uucp.Break.tty_width_hint u in
	if w < 0 then 1 else w

let first_uchar s =
	let result = ref None in
	let exception Found in
	(try
		Uutf.String.fold_utf_8
			(fun () _ dec ->
				(match dec with
				| `Uchar u -> result := Some u
				| `Malformed _ -> ());
				raise Found)
			() s
	with Found -> ());
	!result

let display_width s =
	let total = ref 0 in
	Uuseg_string.fold_utf_8 `Grapheme_cluster
		(fun () cluster ->
			let w = match first_uchar cluster with
				| Some u -> uchar_width u
				| None -> 0
			in
			total := !total + w)
		() s;
	!total

let is_word_char u =
	Uchar.equal u (Uchar.of_char '_')
	|| Uucp.Alpha.is_alphabetic u
	|| Uucp.Gc.general_category u = `Nd

let cluster_is_word c =
	match first_uchar c with
	| Some u -> is_word_char u
	| None -> false

let clusters_array s =
	let result = ref [] in
	Uuseg_string.fold_utf_8 `Grapheme_cluster
		(fun () c -> result := c :: !result)
		() s;
	Array.of_list (List.rev !result)

let next_word_start s idx =
	let cs = clusters_array s in
	let n = Array.length cs in
	let idx = max 0 (min idx n) in
	if idx >= n then n
	else begin
		let started_on_word = cluster_is_word cs.(idx) in
		let i = ref idx in
		while !i < n && cluster_is_word cs.(!i) = started_on_word do incr i done;
		if started_on_word then
			while !i < n && not (cluster_is_word cs.(!i)) do incr i done;
		!i
	end

let prev_word_start s idx =
	let cs = clusters_array s in
	let n = Array.length cs in
	let idx = max 0 (min idx n) in
	if idx = 0 then 0
	else begin
		let i = ref (idx - 1) in
		while !i > 0 && not (cluster_is_word cs.(!i)) do decr i done;
		while !i > 0 && cluster_is_word cs.(!i - 1) do decr i done;
		!i
	end

let truncate_to_width s width =
	if width <= 0 then ""
	else begin
		let buf = Buffer.create (String.length s) in
		let acc = ref 0 in
		let stopped = ref false in
		Uuseg_string.fold_utf_8 `Grapheme_cluster
			(fun () cluster ->
				if !stopped then ()
				else begin
					let w = match first_uchar cluster with
						| Some u -> uchar_width u
						| None -> 0
					in
					if !acc + w > width then stopped := true
					else begin
						Buffer.add_string buf cluster;
						acc := !acc + w
					end
				end)
			() s;
		Buffer.contents buf
	end
