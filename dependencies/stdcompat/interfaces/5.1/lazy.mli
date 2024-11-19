type 'a t = 'a CamlinternalLazy.t
exception Undefined 
external force : 'a t -> 'a = "%lazy_force"
val map : ('a -> 'b) -> 'a t -> 'b t
val is_val : 'a t -> bool
val from_val : 'a -> 'a t
val map_val : ('a -> 'b) -> 'a t -> 'b t
val from_fun : (unit -> 'a) -> 'a t
val force_val : 'a t -> 'a
