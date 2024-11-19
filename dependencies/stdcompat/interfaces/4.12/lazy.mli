type 'a t = 'a CamlinternalLazy.t
exception Undefined 
external force : 'a t -> 'a = "%lazy_force"
val force_val : 'a t -> 'a
val from_fun : (unit -> 'a) -> 'a t
val from_val : 'a -> 'a t
val is_val : 'a t -> bool
val lazy_from_fun : (unit -> 'a) -> 'a t
val lazy_from_val : 'a -> 'a t
val lazy_is_val : 'a t -> bool
