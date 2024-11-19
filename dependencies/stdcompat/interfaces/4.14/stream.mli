type !'a t
exception Failure 
exception Error of string 
val from : (int -> 'a option) -> 'a t
val of_list : 'a list -> 'a t
val of_string : string -> char t
val of_bytes : bytes -> char t
val of_channel : in_channel -> char t
val iter : ('a -> unit) -> 'a t -> unit
val next : 'a t -> 'a
val empty : 'a t -> unit
val peek : 'a t -> 'a option
val junk : 'a t -> unit
val count : 'a t -> int
val npeek : int -> 'a t -> 'a list
val iapp : 'a t -> 'a t -> 'a t
val icons : 'a -> 'a t -> 'a t
val ising : 'a -> 'a t
val lapp : (unit -> 'a t) -> 'a t -> 'a t
val lcons : (unit -> 'a) -> 'a t -> 'a t
val lsing : (unit -> 'a) -> 'a t
val sempty : 'a t
val slazy : (unit -> 'a t) -> 'a t
val dump : ('a -> unit) -> 'a t -> unit
