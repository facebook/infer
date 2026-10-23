val log_error : ('a, unit, string, unit) format4 -> 'a
val append_tr : 'a list -> 'a list -> 'a list
val concat_tr : 'a list list -> 'a list
val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
val range : int -> int -> int list
val tempfd : unit -> Unix.file_descr
