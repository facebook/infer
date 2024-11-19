type !'a t
exception Empty 
val create : unit -> 'a t
val add : 'a -> 'a t -> unit
val push : 'a -> 'a t -> unit
val take : 'a t -> 'a
val take_opt : 'a t -> 'a option
val pop : 'a t -> 'a
val peek : 'a t -> 'a
val peek_opt : 'a t -> 'a option
val top : 'a t -> 'a
val clear : 'a t -> unit
val copy : 'a t -> 'a t
val is_empty : 'a t -> bool
val length : 'a t -> int
val iter : ('a -> unit) -> 'a t -> unit
val fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
val transfer : 'a t -> 'a t -> unit
val to_seq : 'a t -> 'a Seq.t
val add_seq : 'a t -> 'a Seq.t -> unit
val of_seq : 'a Seq.t -> 'a t
