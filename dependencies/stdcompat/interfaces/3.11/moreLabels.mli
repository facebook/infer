module Hashtbl :
sig
  type ('a, 'b) t = ('a, 'b) Hashtbl.t
  val create : int -> ('a, 'b) t
  val clear : ('a, 'b) t -> unit
  val add : ('a, 'b) t -> key:'a -> data:'b -> unit
  val copy : ('a, 'b) t -> ('a, 'b) t
  val find : ('a, 'b) t -> 'a -> 'b
  val find_all : ('a, 'b) t -> 'a -> 'b list
  val mem : ('a, 'b) t -> 'a -> bool
  val remove : ('a, 'b) t -> 'a -> unit
  val replace : ('a, 'b) t -> key:'a -> data:'b -> unit
  val iter : f:(key:'a -> data:'b -> unit) -> ('a, 'b) t -> unit
  val fold : f:(key:'a -> data:'b -> 'c -> 'c) -> ('a, 'b) t -> init:'c -> 'c
  val length : ('a, 'b) t -> int
  module type HashedType  = Hashtbl.HashedType
  module type S  =
    sig
      type key
      and 'a t
      val create : int -> 'a t
      val clear : 'a t -> unit
      val copy : 'a t -> 'a t
      val add : 'a t -> key:key -> data:'a -> unit
      val remove : 'a t -> key -> unit
      val find : 'a t -> key -> 'a
      val find_all : 'a t -> key -> 'a list
      val replace : 'a t -> key:key -> data:'a -> unit
      val mem : 'a t -> key -> bool
      val iter : f:(key:key -> data:'a -> unit) -> 'a t -> unit
      val fold : f:(key:key -> data:'a -> 'b -> 'b) -> 'a t -> init:'b -> 'b
      val length : 'a t -> int
    end
  module Make :
  functor (H : HashedType) ->
    sig
      type key = H.t
      and 'a t
      val create : int -> 'a t
      val clear : 'a t -> unit
      val copy : 'a t -> 'a t
      val add : 'a t -> key:key -> data:'a -> unit
      val remove : 'a t -> key -> unit
      val find : 'a t -> key -> 'a
      val find_all : 'a t -> key -> 'a list
      val replace : 'a t -> key:key -> data:'a -> unit
      val mem : 'a t -> key -> bool
      val iter : f:(key:key -> data:'a -> unit) -> 'a t -> unit
      val fold : f:(key:key -> data:'a -> 'b -> 'b) -> 'a t -> init:'b -> 'b
      val length : 'a t -> int
    end
  val hash : 'a -> int
  external hash_param :
    int -> int -> 'a -> int = "caml_hash_univ_param" "noalloc"
end
module Map :
sig
  module type OrderedType  = Map.OrderedType
  module type S  =
    sig
      type key
      and +'a t
      val empty : 'a t
      val is_empty : 'a t -> bool
      val add : key:key -> data:'a -> 'a t -> 'a t
      val find : key -> 'a t -> 'a
      val remove : key -> 'a t -> 'a t
      val mem : key -> 'a t -> bool
      val iter : f:(key:key -> data:'a -> unit) -> 'a t -> unit
      val map : f:('a -> 'b) -> 'a t -> 'b t
      val mapi : f:(key -> 'a -> 'b) -> 'a t -> 'b t
      val fold : f:(key:key -> data:'a -> 'b -> 'b) -> 'a t -> init:'b -> 'b
      val compare : cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int
      val equal : cmp:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    end
  module Make :
  functor (Ord : OrderedType) ->
    sig
      type key = Ord.t
      and 'a t
      val empty : 'a t
      val is_empty : 'a t -> bool
      val add : key:key -> data:'a -> 'a t -> 'a t
      val find : key -> 'a t -> 'a
      val remove : key -> 'a t -> 'a t
      val mem : key -> 'a t -> bool
      val iter : f:(key:key -> data:'a -> unit) -> 'a t -> unit
      val map : f:('a -> 'b) -> 'a t -> 'b t
      val mapi : f:(key -> 'a -> 'b) -> 'a t -> 'b t
      val fold : f:(key:key -> data:'a -> 'b -> 'b) -> 'a t -> init:'b -> 'b
      val compare : cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int
      val equal : cmp:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    end
end
module Set :
sig
  module type OrderedType  = Set.OrderedType
  module type S  =
    sig
      type elt
      and t
      val empty : t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val diff : t -> t -> t
      val compare : t -> t -> int
      val equal : t -> t -> bool
      val subset : t -> t -> bool
      val iter : f:(elt -> unit) -> t -> unit
      val fold : f:(elt -> 'a -> 'a) -> t -> init:'a -> 'a
      val for_all : f:(elt -> bool) -> t -> bool
      val exists : f:(elt -> bool) -> t -> bool
      val filter : f:(elt -> bool) -> t -> t
      val partition : f:(elt -> bool) -> t -> (t * t)
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val max_elt : t -> elt
      val choose : t -> elt
      val split : elt -> t -> (t * bool * t)
    end
  module Make :
  functor (Ord : OrderedType) ->
    sig
      type elt = Ord.t
      and t
      val empty : t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val diff : t -> t -> t
      val compare : t -> t -> int
      val equal : t -> t -> bool
      val subset : t -> t -> bool
      val iter : f:(elt -> unit) -> t -> unit
      val fold : f:(elt -> 'a -> 'a) -> t -> init:'a -> 'a
      val for_all : f:(elt -> bool) -> t -> bool
      val exists : f:(elt -> bool) -> t -> bool
      val filter : f:(elt -> bool) -> t -> t
      val partition : f:(elt -> bool) -> t -> (t * t)
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val max_elt : t -> elt
      val choose : t -> elt
      val split : elt -> t -> (t * bool * t)
    end
end
