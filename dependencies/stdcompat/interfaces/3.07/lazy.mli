type 'a t = 'a lazy_t
exception Undefined 
val force : 'a t -> 'a
val force_val : 'a t -> 'a
val lazy_from_fun : (unit -> 'a) -> 'a t
val lazy_from_val : 'a -> 'a t
val lazy_is_val : 'a t -> bool
