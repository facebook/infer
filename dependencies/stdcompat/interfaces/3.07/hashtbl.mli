type ('a, 'b) t
val create : int -> ('a, 'b) t
val clear : ('a, 'b) t -> unit
val add : ('a, 'b) t -> 'a -> 'b -> unit
val copy : ('a, 'b) t -> ('a, 'b) t
val find : ('a, 'b) t -> 'a -> 'b
val find_all : ('a, 'b) t -> 'a -> 'b list
val mem : ('a, 'b) t -> 'a -> bool
val remove : ('a, 'b) t -> 'a -> unit
val replace : ('a, 'b) t -> 'a -> 'b -> unit
val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
module type HashedType  =
  sig type t val equal : t -> t -> bool val hash : t -> int end
module type S  =
  sig
    type key
    and 'a t
    val create : int -> 'a t
    val clear : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  end
module Make :
functor (H : HashedType) ->
  sig
    type key = H.t
    and 'a t
    val create : int -> 'a t
    val clear : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  end
val hash : 'a -> int
external hash_param : int -> int -> 'a -> int = "hash_univ_param" "noalloc"
