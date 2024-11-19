type !'a t
val create : int -> 'a t
val length : 'a t -> int
val set : 'a t -> int -> 'a option -> unit
val get : 'a t -> int -> 'a option
val get_copy : 'a t -> int -> 'a option
val check : 'a t -> int -> bool
val fill : 'a t -> int -> int -> 'a option -> unit
val blit : 'a t -> int -> 'a t -> int -> int -> unit
module type S  =
  sig
    type data
    type t
    val create : int -> t
    val clear : t -> unit
    val merge : t -> data -> data
    val add : t -> data -> unit
    val remove : t -> data -> unit
    val find : t -> data -> data
    val find_opt : t -> data -> data option
    val find_all : t -> data -> data list
    val mem : t -> data -> bool
    val iter : (data -> unit) -> t -> unit
    val fold : (data -> 'acc -> 'acc) -> t -> 'acc -> 'acc
    val count : t -> int
    val stats : t -> (int * int * int * int * int * int)
  end
module Make :
functor (H : Hashtbl.HashedType) ->
  sig
    type data = H.t
    type t
    val create : int -> t
    val clear : t -> unit
    val merge : t -> data -> data
    val add : t -> data -> unit
    val remove : t -> data -> unit
    val find : t -> data -> data
    val find_opt : t -> data -> data option
    val find_all : t -> data -> data list
    val mem : t -> data -> bool
    val iter : (data -> unit) -> t -> unit
    val fold : (data -> 'acc -> 'acc) -> t -> 'acc -> 'acc
    val count : t -> int
    val stats : t -> (int * int * int * int * int * int)
  end
