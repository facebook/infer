type !'a t
exception Empty 
val create : unit -> 'a t
val push : 'a -> 'a t -> unit
val pop : 'a t -> 'a
val pop_opt : 'a t -> 'a option
val top : 'a t -> 'a
val top_opt : 'a t -> 'a option
val clear : 'a t -> unit
val copy : 'a t -> 'a t
val is_empty : 'a t -> bool
val length : 'a t -> int
val iter : ('a -> unit) -> 'a t -> unit
val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
val to_seq : 'a t -> 'a Seq.t
val add_seq : 'a t -> 'a Seq.t -> unit
val of_seq : 'a Seq.t -> 'a t
