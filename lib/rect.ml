type t = {
	row  : int;
	col  : int;
	rows : int;
	cols : int;
}

let max0 x = if x < 0 then 0 else x

let make ~row ~col ~rows ~cols =
	{ row; col; rows = max0 rows; cols = max0 cols }

let is_empty r = r.rows = 0 || r.cols = 0

let clamp01 x =
	if x < 0.0 then 0.0
	else if x > 1.0 then 1.0
	else x

let split_h r ~ratio =
	let ratio = clamp01 ratio in
	let top_rows = int_of_float (float_of_int r.rows *. ratio) in
	let top_rows = if top_rows > r.rows then r.rows else top_rows in
	let top = { r with rows = top_rows } in
	let bot = {
		row  = r.row + top_rows;
		col  = r.col;
		rows = r.rows - top_rows;
		cols = r.cols;
	} in
	(top, bot)

let split_v r ~ratio =
	let ratio = clamp01 ratio in
	let left_cols = int_of_float (float_of_int r.cols *. ratio) in
	let left_cols = if left_cols > r.cols then r.cols else left_cols in
	let left = { r with cols = left_cols } in
	let right = {
		row  = r.row;
		col  = r.col + left_cols;
		rows = r.rows;
		cols = r.cols - left_cols;
	} in
	(left, right)

let contains r ~row ~col =
	row >= r.row
	&& row < r.row + r.rows
	&& col >= r.col
	&& col < r.col + r.cols

let intersect a b =
	let row  = if a.row > b.row then a.row else b.row in
	let col  = if a.col > b.col then a.col else b.col in
	let a_end_r = a.row + a.rows in
	let b_end_r = b.row + b.rows in
	let a_end_c = a.col + a.cols in
	let b_end_c = b.col + b.cols in
	let end_r = if a_end_r < b_end_r then a_end_r else b_end_r in
	let end_c = if a_end_c < b_end_c then a_end_c else b_end_c in
	if end_r <= row || end_c <= col then None
	else Some { row; col; rows = end_r - row; cols = end_c - col }

let shrink r ~top ~bottom ~left ~right =
	let top = max0 top in
	let bottom = max0 bottom in
	let left = max0 left in
	let right = max0 right in
	let rows = r.rows - top - bottom in
	let cols = r.cols - left - right in
	if rows <= 0 || cols <= 0 then
		{ row = r.row + top; col = r.col + left; rows = 0; cols = 0 }
	else
		{
			row  = r.row + top;
			col  = r.col + left;
			rows;
			cols;
		}
