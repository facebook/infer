module type S  =
  sig
    type key
    type 'a t
    val create : int -> 'a t
    val clear : 'a t -> unit
    val reset : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_opt : 'a t -> key -> 'a option
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length : 'a t -> int
    val stats : 'a t -> Hashtbl.statistics
    val clean : 'a t -> unit
    val stats_alive : 'a t -> Hashtbl.statistics
  end
module type SeededS  =
  sig
    type key
    type 'a t
    val create : ?random:bool -> int -> 'a t
    val clear : 'a t -> unit
    val reset : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_opt : 'a t -> key -> 'a option
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length : 'a t -> int
    val stats : 'a t -> Hashtbl.statistics
    val clean : 'a t -> unit
    val stats_alive : 'a t -> Hashtbl.statistics
  end
module K1 :
sig
  type ('k, 'd) t
  val create : unit -> ('k, 'd) t
  val get_key : ('k, 'd) t -> 'k option
  val get_key_copy : ('k, 'd) t -> 'k option
  val set_key : ('k, 'd) t -> 'k -> unit
  val unset_key : ('k, 'd) t -> unit
  val check_key : ('k, 'd) t -> bool
  val blit_key : ('k, 'a) t -> ('k, 'b) t -> unit
  val get_data : ('k, 'd) t -> 'd option
  val get_data_copy : ('k, 'd) t -> 'd option
  val set_data : ('k, 'd) t -> 'd -> unit
  val unset_data : ('k, 'd) t -> unit
  val check_data : ('k, 'd) t -> bool
  val blit_data : ('a, 'd) t -> ('b, 'd) t -> unit
  module Make :
  functor (H : Hashtbl.HashedType) ->
    sig
      type key = H.t
      type 'a t
      val create : int -> 'a t
      val clear : 'a t -> unit
      val reset : 'a t -> unit
      val copy : 'a t -> 'a t
      val add : 'a t -> key -> 'a -> unit
      val remove : 'a t -> key -> unit
      val find : 'a t -> key -> 'a
      val find_opt : 'a t -> key -> 'a option
      val find_all : 'a t -> key -> 'a list
      val replace : 'a t -> key -> 'a -> unit
      val mem : 'a t -> key -> bool
      val iter : (key -> 'a -> unit) -> 'a t -> unit
      val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
      val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
      val length : 'a t -> int
      val stats : 'a t -> Hashtbl.statistics
      val clean : 'a t -> unit
      val stats_alive : 'a t -> Hashtbl.statistics
    end
  module MakeSeeded :
  functor (H : Hashtbl.SeededHashedType) ->
    sig
      type key = H.t
      type 'a t
      val create : ?random:bool -> int -> 'a t
      val clear : 'a t -> unit
      val reset : 'a t -> unit
      val copy : 'a t -> 'a t
      val add : 'a t -> key -> 'a -> unit
      val remove : 'a t -> key -> unit
      val find : 'a t -> key -> 'a
      val find_opt : 'a t -> key -> 'a option
      val find_all : 'a t -> key -> 'a list
      val replace : 'a t -> key -> 'a -> unit
      val mem : 'a t -> key -> bool
      val iter : (key -> 'a -> unit) -> 'a t -> unit
      val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
      val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
      val length : 'a t -> int
      val stats : 'a t -> Hashtbl.statistics
      val clean : 'a t -> unit
      val stats_alive : 'a t -> Hashtbl.statistics
    end
end
module K2 :
sig
  type ('k1, 'k2, 'd) t
  val create : unit -> ('k1, 'k2, 'd) t
  val get_key1 : ('k1, 'k2, 'd) t -> 'k1 option
  val get_key1_copy : ('k1, 'k2, 'd) t -> 'k1 option
  val set_key1 : ('k1, 'k2, 'd) t -> 'k1 -> unit
  val unset_key1 : ('k1, 'k2, 'd) t -> unit
  val check_key1 : ('k1, 'k2, 'd) t -> bool
  val get_key2 : ('k1, 'k2, 'd) t -> 'k2 option
  val get_key2_copy : ('k1, 'k2, 'd) t -> 'k2 option
  val set_key2 : ('k1, 'k2, 'd) t -> 'k2 -> unit
  val unset_key2 : ('k1, 'k2, 'd) t -> unit
  val check_key2 : ('k1, 'k2, 'd) t -> bool
  val blit_key1 : ('k1, 'a, 'b) t -> ('k1, 'c, 'd) t -> unit
  val blit_key2 : ('a, 'k2, 'b) t -> ('c, 'k2, 'd) t -> unit
  val blit_key12 : ('k1, 'k2, 'a) t -> ('k1, 'k2, 'b) t -> unit
  val get_data : ('k1, 'k2, 'd) t -> 'd option
  val get_data_copy : ('k1, 'k2, 'd) t -> 'd option
  val set_data : ('k1, 'k2, 'd) t -> 'd -> unit
  val unset_data : ('k1, 'k2, 'd) t -> unit
  val check_data : ('k1, 'k2, 'd) t -> bool
  val blit_data : ('k1, 'k2, 'd) t -> ('k1, 'k2, 'd) t -> unit
  module Make :
  functor (H1 : Hashtbl.HashedType) ->
    functor (H2 : Hashtbl.HashedType) ->
      sig
        type key = (H1.t * H2.t)
        type 'a t
        val create : int -> 'a t
        val clear : 'a t -> unit
        val reset : 'a t -> unit
        val copy : 'a t -> 'a t
        val add : 'a t -> key -> 'a -> unit
        val remove : 'a t -> key -> unit
        val find : 'a t -> key -> 'a
        val find_opt : 'a t -> key -> 'a option
        val find_all : 'a t -> key -> 'a list
        val replace : 'a t -> key -> 'a -> unit
        val mem : 'a t -> key -> bool
        val iter : (key -> 'a -> unit) -> 'a t -> unit
        val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
        val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
        val length : 'a t -> int
        val stats : 'a t -> Hashtbl.statistics
        val clean : 'a t -> unit
        val stats_alive : 'a t -> Hashtbl.statistics
      end
  module MakeSeeded :
  functor (H1 : Hashtbl.SeededHashedType) ->
    functor (H2 : Hashtbl.SeededHashedType) ->
      sig
        type key = (H1.t * H2.t)
        type 'a t
        val create : ?random:bool -> int -> 'a t
        val clear : 'a t -> unit
        val reset : 'a t -> unit
        val copy : 'a t -> 'a t
        val add : 'a t -> key -> 'a -> unit
        val remove : 'a t -> key -> unit
        val find : 'a t -> key -> 'a
        val find_opt : 'a t -> key -> 'a option
        val find_all : 'a t -> key -> 'a list
        val replace : 'a t -> key -> 'a -> unit
        val mem : 'a t -> key -> bool
        val iter : (key -> 'a -> unit) -> 'a t -> unit
        val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
        val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
        val length : 'a t -> int
        val stats : 'a t -> Hashtbl.statistics
        val clean : 'a t -> unit
        val stats_alive : 'a t -> Hashtbl.statistics
      end
end
module Kn :
sig
  type ('k, 'd) t
  val create : int -> ('k, 'd) t
  val get_key : ('k, 'd) t -> int -> 'k option
  val get_key_copy : ('k, 'd) t -> int -> 'k option
  val set_key : ('k, 'd) t -> int -> 'k -> unit
  val unset_key : ('k, 'd) t -> int -> unit
  val check_key : ('k, 'd) t -> int -> bool
  val blit_key : ('k, 'a) t -> int -> ('k, 'b) t -> int -> int -> unit
  val get_data : ('k, 'd) t -> 'd option
  val get_data_copy : ('k, 'd) t -> 'd option
  val set_data : ('k, 'd) t -> 'd -> unit
  val unset_data : ('k, 'd) t -> unit
  val check_data : ('k, 'd) t -> bool
  val blit_data : ('k, 'd) t -> ('k, 'd) t -> unit
  module Make :
  functor (H : Hashtbl.HashedType) ->
    sig
      type key = H.t array
      type 'a t
      val create : int -> 'a t
      val clear : 'a t -> unit
      val reset : 'a t -> unit
      val copy : 'a t -> 'a t
      val add : 'a t -> key -> 'a -> unit
      val remove : 'a t -> key -> unit
      val find : 'a t -> key -> 'a
      val find_opt : 'a t -> key -> 'a option
      val find_all : 'a t -> key -> 'a list
      val replace : 'a t -> key -> 'a -> unit
      val mem : 'a t -> key -> bool
      val iter : (key -> 'a -> unit) -> 'a t -> unit
      val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
      val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
      val length : 'a t -> int
      val stats : 'a t -> Hashtbl.statistics
      val clean : 'a t -> unit
      val stats_alive : 'a t -> Hashtbl.statistics
    end
  module MakeSeeded :
  functor (H : Hashtbl.SeededHashedType) ->
    sig
      type key = H.t array
      type 'a t
      val create : ?random:bool -> int -> 'a t
      val clear : 'a t -> unit
      val reset : 'a t -> unit
      val copy : 'a t -> 'a t
      val add : 'a t -> key -> 'a -> unit
      val remove : 'a t -> key -> unit
      val find : 'a t -> key -> 'a
      val find_opt : 'a t -> key -> 'a option
      val find_all : 'a t -> key -> 'a list
      val replace : 'a t -> key -> 'a -> unit
      val mem : 'a t -> key -> bool
      val iter : (key -> 'a -> unit) -> 'a t -> unit
      val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
      val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
      val length : 'a t -> int
      val stats : 'a t -> Hashtbl.statistics
      val clean : 'a t -> unit
      val stats_alive : 'a t -> Hashtbl.statistics
    end
end
module GenHashTable :
sig
  type equal =
    | ETrue 
    | EFalse 
    | EDead 
  module MakeSeeded :
  functor (H :
    sig
      type t
      type 'a container
      val hash : int -> t -> int
      val equal : 'a container -> t -> equal
      val create : t -> 'a -> 'a container
      val get_key : 'a container -> t option
      val get_data : 'a container -> 'a option
      val set_key_data : 'a container -> t -> 'a -> unit
      val check_key : 'a container -> bool
    end) ->
    sig
      type key = H.t
      type 'a t
      val create : ?random:bool -> int -> 'a t
      val clear : 'a t -> unit
      val reset : 'a t -> unit
      val copy : 'a t -> 'a t
      val add : 'a t -> key -> 'a -> unit
      val remove : 'a t -> key -> unit
      val find : 'a t -> key -> 'a
      val find_opt : 'a t -> key -> 'a option
      val find_all : 'a t -> key -> 'a list
      val replace : 'a t -> key -> 'a -> unit
      val mem : 'a t -> key -> bool
      val iter : (key -> 'a -> unit) -> 'a t -> unit
      val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
      val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
      val length : 'a t -> int
      val stats : 'a t -> Hashtbl.statistics
      val clean : 'a t -> unit
      val stats_alive : 'a t -> Hashtbl.statistics
    end
end
