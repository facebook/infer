external length : 'a array -> int = "%array_length"
external get : 'a array -> int -> 'a = "%array_safe_get"
external set : 'a array -> int -> 'a -> unit = "%array_safe_set"
external make : int -> 'a -> 'a array = "caml_make_vect"
external create : int -> 'a -> 'a array = "caml_make_vect"[@@ocaml.deprecated
                                                            "Use Array.make instead."]
val init : int -> (int -> 'a) -> 'a array
val make_matrix : int -> int -> 'a -> 'a array array
val create_matrix : int -> int -> 'a -> 'a array array[@@ocaml.deprecated
                                                        "Use Array.make_matrix instead."]
val append : 'a array -> 'a array -> 'a array
val concat : 'a array list -> 'a array
val sub : 'a array -> int -> int -> 'a array
val copy : 'a array -> 'a array
val fill : 'a array -> int -> int -> 'a -> unit
val blit : 'a array -> int -> 'a array -> int -> int -> unit
val to_list : 'a array -> 'a list
val of_list : 'a list -> 'a array
val iter : ('a -> unit) -> 'a array -> unit
val map : ('a -> 'b) -> 'a array -> 'b array
val iteri : (int -> 'a -> unit) -> 'a array -> unit
val mapi : (int -> 'a -> 'b) -> 'a array -> 'b array
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
val fold_right : ('b -> 'a -> 'a) -> 'b array -> 'a -> 'a
external make_float : int -> float array = "caml_make_float_vect"
val sort : ('a -> 'a -> int) -> 'a array -> unit
val stable_sort : ('a -> 'a -> int) -> 'a array -> unit
val fast_sort : ('a -> 'a -> int) -> 'a array -> unit
external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
external unsafe_set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"
