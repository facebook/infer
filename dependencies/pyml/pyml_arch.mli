type t = Windows | Mac | Unix

val os : t

val fd_of_int: int -> Unix.file_descr
