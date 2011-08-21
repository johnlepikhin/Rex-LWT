
open Rex_type

module Group = struct
	type t = {
		pos : int;
		mutable length : int;
	}

	let make pos = {
		pos = pos;
		length = 0;
	}

	let incr t =
		t.length <- t.length + 1;
end

module R = struct
	type t = {
		s : Buffer.t;
		mutable groups : Group.t list;
	}

	let make () = {
		s = Buffer.create 500;
		groups = [];
	}

	let new_group t pos =
		let r = Group.make pos in
		t.groups <- r :: t.groups;
		r

	let add_char t = Buffer.add_char t.s

	let length t = Buffer.length t.s

	let nth t = Buffer.nth t.s

	let release t = {
		s = t.s;
		groups = List.rev t.groups;
	}

	let group t g = Buffer.sub t.s g.Group.pos g.Group.length

	let group_nth t n =
		let g = List.nth t.groups n in
		group t g
end

module W = struct
	type t = {
		get : unit -> char Lwt.t;
		r : R.t;
		mutable pos : int option;
	}

	let make get = {
		get = get;
		r = R.make ();
		pos = None;
	}

	let get t =
		let real_get () =
			lwt r = t.get () in
			R.add_char t.r r;
			Lwt.return r
		in
		match t.pos with
			| None -> real_get ()
			| Some pos ->
				if pos = R.length t.r then
				begin
					t.pos <- None;
					real_get ()
				end
				else
				begin
					let r = R.nth t.r pos in
					t.pos <- Some (pos+1);
					Lwt.return r
				end

	let moveback t n =
		t.pos <- Some (match t.pos with
			| None -> (R.length t.r) - n
			| Some p -> p - n
		)

	let pos t =
		match t.pos with
			| None -> R.length t.r
			| Some p -> p

	let snapshot = pos

	let rollback t s =
		let pos = pos t in
		let len = R.length t.r in
		let offset = len - pos - 1 in
		moveback t offset

	let new_group t = R.new_group t.r (pos t)
end

let apply ~get rex =
	let w = W.make get in
	let add_to_groups = List.iter Group.incr in
	let rec loop ~cur_iter ~groups lst =
		let success () =
			add_to_groups groups;
			loop ~cur_iter:(cur_iter+1) ~groups lst
		in
		match lst with
			| [] -> Lwt.return true
			| hd :: tl ->
				if cur_iter = hd.rep_max then
					loop ~cur_iter:0 ~groups tl
				else
				begin
					match hd.rep_what with
						| Char c ->
							begin
								try_lwt
									lwt next = W.get w in
									if c = next then
										success ()
									else
									begin
										W.moveback w 1;
										if hd.rep_min > cur_iter then
											Lwt.return false
										else
											loop ~cur_iter:0 ~groups tl
									end
								with
									| EOS when hd.rep_min <= cur_iter -> loop ~cur_iter:0 ~groups tl
									| EOS -> Lwt.return false
							end
						| AnyChar ->
							begin
								try_lwt
									lwt _ = W.get w in
									success ()
								with
									| EOS when hd.rep_min <= cur_iter -> loop ~cur_iter:0 ~groups tl
									| EOS -> Lwt.return false
							end
						| CharSelect c ->
							begin
								try_lwt
									lwt next = W.get w in
									let contains = List.mem next c.sel_list in
									if (contains && not c.sel_negative) || (not contains && c.sel_negative) then
										success ()
									else
									begin
										W.moveback w 1;
										if hd.rep_min > cur_iter then
											Lwt.return false
										else
											loop ~cur_iter:0 ~groups tl
									end
								with
									| EOS when hd.rep_min <= cur_iter -> loop ~cur_iter:0 ~groups tl
									| EOS -> Lwt.return false
							end
						| Group g ->
							let grp = W.new_group w in
							let rec loop_group cur_iter =
								if cur_iter = hd.rep_max then
									Lwt.return true
								else
								begin
									let snapshot = W.snapshot w in
									lwt r = loop ~cur_iter:0 ~groups:(grp :: groups) g in
									if r = false then
										if cur_iter >= hd.rep_min then
										begin
											W.rollback w snapshot;
											Lwt.return true
										end
										else
											Lwt.return false
									else
										loop_group (cur_iter+1)
								end
							in
							lwt res = loop_group 0 in
							if res = false then
								Lwt.return false
							else
								loop ~cur_iter:0 ~groups tl
				end
	in
	lwt result = loop ~cur_iter:0 ~groups:[] rex in
	let r =
		if result then
			Some (R.release w.W.r)
		else
			None
	in
	Lwt.return r
