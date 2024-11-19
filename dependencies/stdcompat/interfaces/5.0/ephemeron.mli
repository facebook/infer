module type S  =
  sig
    type key
    type !'a t
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
    val length : 'a t -> int
    val stats : 'a t -> Hashtbl.statistics
    val add_seq : 'a t -> (key * 'a) Seq.t -> unit
    val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
    val of_seq : (key * 'a) Seq.t -> 'a t
    val clean : 'a t -> unit
    val stats_alive : 'a t -> Hashtbl.statistics
  end
module type SeededS  =
  sig
    type key
    type !'a t
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
    val length : 'a t -> int
    val stats : 'a t -> Hashtbl.statistics
    val add_seq : 'a t -> (key * 'a) Seq.t -> unit
    val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
    val of_seq : (key * 'a) Seq.t -> 'a t
    val clean : 'a t -> unit
    val stats_alive : 'a t -> Hashtbl.statistics
  end
module K1 :
sig
  type ('k, 'd) t
  val make : 'k -> 'd -> ('k, 'd) t
  val query : ('k, 'd) t -> 'k -> 'd option
  module Make :
  functor (H : Hashtbl.HashedType) ->
    sig
      type key = H.t
      type !'a t
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
      val length : 'a t -> int
      val stats : 'a t -> Hashtbl.statistics
      val add_seq : 'a t -> (key * 'a) Seq.t -> unit
      val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
      val of_seq : (key * 'a) Seq.t -> 'a t
      val clean : 'a t -> unit
      val stats_alive : 'a t -> Hashtbl.statistics
    end
  module MakeSeeded :
  functor (H : Hashtbl.SeededHashedType) ->
    sig
      type key = H.t
      type !'a t
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
      val length : 'a t -> int
      val stats : 'a t -> Hashtbl.statistics
      val add_seq : 'a t -> (key * 'a) Seq.t -> unit
      val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
      val of_seq : (key * 'a) Seq.t -> 'a t
      val clean : 'a t -> unit
      val stats_alive : 'a t -> Hashtbl.statistics
    end
  module Bucket :
  sig
    type ('k, 'd) t
    val make : unit -> ('k, 'd) t
    val add : ('k, 'd) t -> 'k -> 'd -> unit
    val remove : ('k, 'd) t -> 'k -> unit
    val find : ('k, 'd) t -> 'k -> 'd option
    val length : ('k, 'd) t -> int
    val clear : ('k, 'd) t -> unit
  end
end
module K2 :
sig
  type ('k1, 'k2, 'd) t
  val make : 'k1 -> 'k2 -> 'd -> ('k1, 'k2, 'd) t
  val query : ('k1, 'k2, 'd) t -> 'k1 -> 'k2 -> 'd option
  module Make :
  functor (H1 : Hashtbl.HashedType) ->
    functor (H2 : Hashtbl.HashedType) ->
      sig
        type key = (H1.t * H2.t)
        type !'a t
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
        val length : 'a t -> int
        val stats : 'a t -> Hashtbl.statistics
        val add_seq : 'a t -> (key * 'a) Seq.t -> unit
        val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
        val of_seq : (key * 'a) Seq.t -> 'a t
        val clean : 'a t -> unit
        val stats_alive : 'a t -> Hashtbl.statistics
      end
  module MakeSeeded :
  functor (H1 : Hashtbl.SeededHashedType) ->
    functor (H2 : Hashtbl.SeededHashedType) ->
      sig
        type key = (H1.t * H2.t)
        type !'a t
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
        val length : 'a t -> int
        val stats : 'a t -> Hashtbl.statistics
        val add_seq : 'a t -> (key * 'a) Seq.t -> unit
        val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
        val of_seq : (key * 'a) Seq.t -> 'a t
        val clean : 'a t -> unit
        val stats_alive : 'a t -> Hashtbl.statistics
      end
  module Bucket :
  sig
    type ('k1, 'k2, 'd) t
    val make : unit -> ('k1, 'k2, 'd) t
    val add : ('k1, 'k2, 'd) t -> 'k1 -> 'k2 -> 'd -> unit
    val remove : ('k1, 'k2, 'd) t -> 'k1 -> 'k2 -> unit
    val find : ('k1, 'k2, 'd) t -> 'k1 -> 'k2 -> 'd option
    val length : ('k1, 'k2, 'd) t -> int
    val clear : ('k1, 'k2, 'd) t -> unit
  end
end
module Kn :
sig
  type ('k, 'd) t
  val make : 'k array -> 'd -> ('k, 'd) t
  val query : ('k, 'd) t -> 'k array -> 'd option
  module Make :
  functor (H : Hashtbl.HashedType) ->
    sig
      type key = H.t array
      type !'a t
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
      val length : 'a t -> int
      val stats : 'a t -> Hashtbl.statistics
      val add_seq : 'a t -> (key * 'a) Seq.t -> unit
      val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
      val of_seq : (key * 'a) Seq.t -> 'a t
      val clean : 'a t -> unit
      val stats_alive : 'a t -> Hashtbl.statistics
    end
  module MakeSeeded :
  functor (H : Hashtbl.SeededHashedType) ->
    sig
      type key = H.t array
      type !'a t
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
      val length : 'a t -> int
      val stats : 'a t -> Hashtbl.statistics
      val add_seq : 'a t -> (key * 'a) Seq.t -> unit
      val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
      val of_seq : (key * 'a) Seq.t -> 'a t
      val clean : 'a t -> unit
      val stats_alive : 'a t -> Hashtbl.statistics
    end
  module Bucket :
  sig
    type ('k, 'd) t
    val make : unit -> ('k, 'd) t
    val add : ('k, 'd) t -> 'k array -> 'd -> unit
    val remove : ('k, 'd) t -> 'k array -> unit
    val find : ('k, 'd) t -> 'k array -> 'd option
    val length : ('k, 'd) t -> int
    val clear : ('k, 'd) t -> unit
  end
end
