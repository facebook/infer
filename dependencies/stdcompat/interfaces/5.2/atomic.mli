type !'a t
val make : 'a -> 'a t
val make_contended : 'a -> 'a t
val get : 'a t -> 'a
val set : 'a t -> 'a -> unit
val exchange : 'a t -> 'a -> 'a
val compare_and_set : 'a t -> 'a -> 'a -> bool
val fetch_and_add : int t -> int -> int
val incr : int t -> unit
val decr : int t -> unit
