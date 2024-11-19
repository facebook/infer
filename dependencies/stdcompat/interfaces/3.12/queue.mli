type 'a t
exception Empty 
val create : unit -> 'a t
val add : 'a -> 'a t -> unit
val push : 'a -> 'a t -> unit
val take : 'a t -> 'a
val pop : 'a t -> 'a
val peek : 'a t -> 'a
val top : 'a t -> 'a
val clear : 'a t -> unit
val copy : 'a t -> 'a t
val is_empty : 'a t -> bool
val length : 'a t -> int
val iter : ('a -> unit) -> 'a t -> unit
val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val transfer : 'a t -> 'a t -> unit
