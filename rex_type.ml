type group = t list

and select = {
	sel_list : char list;
	sel_negative : bool;
}

and t = {
	rep_min : int;
	rep_max : int;
	rep_what : part;
}

and part =
	| Char of char
	| AnyChar
	| Group of group
	| CharSelect of select

type rex = t list

exception EOS
