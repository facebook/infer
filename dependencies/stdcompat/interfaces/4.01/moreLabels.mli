module Hashtbl :
sig
  type ('a, 'b) t = ('a, 'b) Hashtbl.t
  val create : ?random:bool -> int -> ('a, 'b) t
  val clear : ('a, 'b) t -> unit
  val reset : ('a, 'b) t -> unit
  val copy : ('a, 'b) t -> ('a, 'b) t
  val add : ('a, 'b) t -> key:'a -> data:'b -> unit
  val find : ('a, 'b) t -> 'a -> 'b
  val find_all : ('a, 'b) t -> 'a -> 'b list
  val mem : ('a, 'b) t -> 'a -> bool
  val remove : ('a, 'b) t -> 'a -> unit
  val replace : ('a, 'b) t -> key:'a -> data:'b -> unit
  val iter : f:(key:'a -> data:'b -> unit) -> ('a, 'b) t -> unit
  val fold : f:(key:'a -> data:'b -> 'c -> 'c) -> ('a, 'b) t -> init:'c -> 'c
  val length : ('a, 'b) t -> int
  val randomize : unit -> unit
  type statistics = Hashtbl.statistics
  val stats : ('a, 'b) t -> statistics
  module type HashedType  = Hashtbl.HashedType
  module type SeededHashedType  = Hashtbl.SeededHashedType
  module type S  =
    sig
      type key
      and 'a t
      val create : int -> 'a t
      val clear : 'a t -> unit
      val reset : 'a t -> unit
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
      val stats : 'a t -> statistics
    end
  module type SeededS  =
    sig
      type key
      and 'a t
      val create : ?random:bool -> int -> 'a t
      val clear : 'a t -> unit
      val reset : 'a t -> unit
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
      val stats : 'a t -> statistics
    end
  module Make :
  functor (H : HashedType) ->
    sig
      type key = H.t
      and 'a t
      val create : int -> 'a t
      val clear : 'a t -> unit
      val reset : 'a t -> unit
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
      val stats : 'a t -> statistics
    end
  module MakeSeeded :
  functor (H : SeededHashedType) ->
    sig
      type key = H.t
      and 'a t
      val create : ?random:bool -> int -> 'a t
      val clear : 'a t -> unit
      val reset : 'a t -> unit
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
      val stats : 'a t -> statistics
    end
  val hash : 'a -> int
  val seeded_hash : int -> 'a -> int
  val hash_param : int -> int -> 'a -> int
  val seeded_hash_param : int -> int -> int -> 'a -> int
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
      val mem : key -> 'a t -> bool
      val add : key:key -> data:'a -> 'a t -> 'a t
      val singleton : key -> 'a -> 'a t
      val remove : key -> 'a t -> 'a t
      val merge :
        f:(key -> 'a option -> 'b option -> 'c option) ->
          'a t -> 'b t -> 'c t
      val compare : cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int
      val equal : cmp:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
      val iter : f:(key:key -> data:'a -> unit) -> 'a t -> unit
      val fold : f:(key:key -> data:'a -> 'b -> 'b) -> 'a t -> init:'b -> 'b
      val for_all : f:(key -> 'a -> bool) -> 'a t -> bool
      val exists : f:(key -> 'a -> bool) -> 'a t -> bool
      val filter : f:(key -> 'a -> bool) -> 'a t -> 'a t
      val partition : f:(key -> 'a -> bool) -> 'a t -> ('a t * 'a t)
      val cardinal : 'a t -> int
      val bindings : 'a t -> (key * 'a) list
      val min_binding : 'a t -> (key * 'a)
      val max_binding : 'a t -> (key * 'a)
      val choose : 'a t -> (key * 'a)
      val split : key -> 'a t -> ('a t * 'a option * 'a t)
      val find : key -> 'a t -> 'a
      val map : f:('a -> 'b) -> 'a t -> 'b t
      val mapi : f:(key -> 'a -> 'b) -> 'a t -> 'b t
    end
  module Make :
  functor (Ord : OrderedType) ->
    sig
      type key = Ord.t
      and 'a t
      val empty : 'a t
      val is_empty : 'a t -> bool
      val mem : key -> 'a t -> bool
      val add : key:key -> data:'a -> 'a t -> 'a t
      val singleton : key -> 'a -> 'a t
      val remove : key -> 'a t -> 'a t
      val merge :
        f:(key -> 'a option -> 'b option -> 'c option) ->
          'a t -> 'b t -> 'c t
      val compare : cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int
      val equal : cmp:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
      val iter : f:(key:key -> data:'a -> unit) -> 'a t -> unit
      val fold : f:(key:key -> data:'a -> 'b -> 'b) -> 'a t -> init:'b -> 'b
      val for_all : f:(key -> 'a -> bool) -> 'a t -> bool
      val exists : f:(key -> 'a -> bool) -> 'a t -> bool
      val filter : f:(key -> 'a -> bool) -> 'a t -> 'a t
      val partition : f:(key -> 'a -> bool) -> 'a t -> ('a t * 'a t)
      val cardinal : 'a t -> int
      val bindings : 'a t -> (key * 'a) list
      val min_binding : 'a t -> (key * 'a)
      val max_binding : 'a t -> (key * 'a)
      val choose : 'a t -> (key * 'a)
      val split : key -> 'a t -> ('a t * 'a option * 'a t)
      val find : key -> 'a t -> 'a
      val map : f:('a -> 'b) -> 'a t -> 'b t
      val mapi : f:(key -> 'a -> 'b) -> 'a t -> 'b t
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
      val find : elt -> t -> elt
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
      val find : elt -> t -> elt
    end
end
