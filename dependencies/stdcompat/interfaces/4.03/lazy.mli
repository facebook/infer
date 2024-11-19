type 'a t = 'a lazy_t
exception Undefined 
external force : 'a t -> 'a = "%lazy_force"
val force_val : 'a t -> 'a
val from_fun : (unit -> 'a) -> 'a t
val from_val : 'a -> 'a t
val is_val : 'a t -> bool
val lazy_from_fun : (unit -> 'a) -> 'a t[@@ocaml.deprecated
                                          "Use Lazy.from_fun instead."]
val lazy_from_val : 'a -> 'a t[@@ocaml.deprecated
                                "Use Lazy.from_val instead."]
val lazy_is_val : 'a t -> bool[@@ocaml.deprecated "Use Lazy.is_val instead."]
