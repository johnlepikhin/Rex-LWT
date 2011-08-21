
open Rex_type

let part min max c = {
	rep_min = min;
	rep_max = max;
	rep_what = c;
}

let eof _ = raise EOS

let f_loop ~s ~pos ~slen ~eof f =
	if pos = slen then
		eof ()
	else
		let c = s.[pos] in
		let pos = pos + 1 in
		f ~pos c

let rec in_select =
	fun ~s ~slen ~pos ->
		let startpos = pos in
		let rec loop ~chars ~escaped ~negative pos =
			let f ~pos c =
				let add () = loop ~chars:(c :: chars) ~escaped:false ~negative pos in
				if escaped then
					add ()
				else
					match c with
						| ']' -> 
							let r = {
								sel_list = List.rev chars;
								sel_negative = negative;
							} in
							pos, r
						| '\\' -> loop ~chars ~escaped:true ~negative pos
						| '^' ->
							if pos - 1 = startpos then
								loop ~chars ~escaped:false ~negative:true pos
							else
								add ()
						| c -> add ()
			in
			f_loop ~s ~pos ~slen ~eof f
		in
		loop ~chars:[] ~escaped:false ~negative:false pos

and in_repeater =
	fun ~s ~slen ~pos ->
		let min = Buffer.create 10 in
		let max = Buffer.create 10 in
		let p b = int_of_string (Buffer.contents b) in
		let rec loop ~what pos =
			let f ~pos c =
				match (what, c) with
					| 0, ',' -> loop ~what:1 pos
					| 0, c -> Buffer.add_char min c; loop ~what pos
					| 1, '}' -> pos, p min, p max
					| 1, c -> Buffer.add_char max c; loop ~what pos
					| _ -> eof ()
			in
			f_loop ~s ~pos ~slen ~eof f
		in
		loop ~what:0 pos

and in_group =
	fun ~s ~slen ~pos ~is_group ->
		let rec loop ~r ?(escaped=false) pos =
			let eof () = pos, r in
			let f ~pos c =
				let add v pos = loop ~r:(part 1 1 v :: r) pos in
				let repeater min max pos =
					match r with
						| [] -> add (Char c) pos
						| hd :: tl ->
							let v = {
								rep_min = min;
								rep_max = max;
								rep_what = hd.rep_what;
							} in
							loop ~r:(v :: tl) pos
				in
				if escaped then
					add (Char c) pos
				else
					match c with
						| '.' -> add AnyChar pos
						| '\\' ->
							loop ~r ~escaped:true pos
						| ')' ->
							if is_group then
								pos, r
							else
								add (Char ')') pos
						| '(' ->
							let (pos, group) = in_group ~s ~slen ~pos ~is_group:true in
							add (Group (List.rev group)) pos;
						| '[' ->
							let (pos, sel) = in_select ~s ~slen ~pos in
							add (CharSelect sel) pos;
						| '+' -> repeater 1 max_int pos
						| '*' -> repeater 0 max_int pos
						| '?' -> repeater 0 1 pos
						| '{' ->
							let (pos, min, max) = in_repeater ~s ~slen ~pos in
							repeater min max pos
						| c ->
							add (Char c) pos
			in
			f_loop ~s ~pos ~slen ~eof f
		in
		let (pos, r) = loop ~r:[] pos in
		pos, r

let regexp s =
	let (_, r) = in_group ~s ~slen:(String.length s) ~is_group:false ~pos:0 in
	List.rev r

