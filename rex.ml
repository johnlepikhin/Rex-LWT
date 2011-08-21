
open Rex_type

type regexp = Rex_type.group

type result = Rex_apply.R.t

type 'a concurrent_list = (string * 'a) list

type get = unit -> char Lwt.t

exception EOS = Rex_type.EOS

let regexp = Rex_parser.regexp

let apply = Rex_apply.apply

let group = Rex_apply.R.group

let nth = Rex_apply.R.group_nth

module ConcurrentApply = struct
	module Entry = struct

		type 'a t = {
			rex : Rex_type.rex;
			res : 'a;
		}

		let make s res = {
			rex = Rex_parser.regexp s;
			res = res;
		}
	end

	type 'a t = {
		length : int;
		lst : 'a Entry.t list;
	}

	exception Found

	type event =
		| Char of char
		| Exn of exn

	let prepare lst = {
		length = List.length lst;
		lst = List.map (fun (s, f) -> Entry.make s f) lst;
	}

	let apply ~get lst =
		let active = ref lst.length in
		let waiting = ref 0 in
		let found = ref None in

		let conditions = List.map (fun _ -> Lwt_condition.create ()) lst.lst in
		let notify v = List.iter (fun c -> Lwt_condition.signal c v) conditions in

		let real_get () =
			waiting := 0;
			try_lwt
				lwt c = get () in
				notify (Char c);
				Lwt.return c
			with
				| e ->
					notify (Exn e);
					Lwt.fail e
		in

		let get condition () =
			if !found <> None then
			begin
				notify (Exn Found);
				Lwt.fail Found
			end
			else
			begin
				incr waiting;
				if !waiting = !active then
					real_get ()
				else
				begin
					lwt r = Lwt_condition.wait condition in
					match r with
						| Exn e -> Lwt.fail e
						| Char r ->
							Lwt.return r
				end
			end
		in

		let make_thread e condition =
			try_lwt
				lwt r = apply ~get:(get condition) e.Entry.rex in
				lwt _ = match r with
						| None ->
							decr active;
							if !active = !waiting then
								lwt _ = real_get () in
								Lwt.return ()
							else
								Lwt.return ()
						| Some r ->
							found := Some (e.Entry.res, r);
							notify (Exn Found);
							Lwt.return ()
				in
				Lwt.return ()
			with
				| Found ->
					notify (Exn Found);
					Lwt.return ()
		in

		let threads = List.map2 make_thread lst.lst conditions in

		lwt _ = Lwt.join threads in
		Lwt.return !found
end

type 'a concurrent = 'a ConcurrentApply.t

let concurrent_prepare = ConcurrentApply.prepare

let concurrent_apply = ConcurrentApply.apply
