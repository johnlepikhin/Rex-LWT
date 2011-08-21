
type regexp

type result

type 'a concurrent

type 'a concurrent_list = (string * 'a) list

type get = unit -> char Lwt.t

exception EOS

val regexp: string -> regexp

val apply: get : get -> regexp -> result option Lwt.t

val concurrent_prepare: 'a concurrent_list -> 'a concurrent

val concurrent_apply: get : get -> 'a concurrent -> ('a * result) option Lwt.t

val nth: result -> int -> string
