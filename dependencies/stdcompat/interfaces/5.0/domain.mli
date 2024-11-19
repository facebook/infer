type !'a t
val spawn : (unit -> 'a) -> 'a t
val join : 'a t -> 'a
type id = private int
val get_id : 'a t -> id
val self : unit -> id
val before_first_spawn : (unit -> unit) -> unit
val at_exit : (unit -> unit) -> unit
val cpu_relax : unit -> unit
val is_main_domain : unit -> bool
val recommended_domain_count : unit -> int
module DLS :
sig
  type 'a key
  val new_key : ?split_from_parent:('a -> 'a) -> (unit -> 'a) -> 'a key
  val get : 'a key -> 'a
  val set : 'a key -> 'a -> unit
end
