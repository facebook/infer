external length : 'a array -> int = "%array_length"
external get : 'a array -> int -> 'a = "%array_safe_get"
external set : 'a array -> int -> 'a -> unit = "%array_safe_set"
external make : int -> 'a -> 'a array = "caml_make_vect"
external create : int -> 'a -> 'a array = "caml_make_vect"
val init : int -> f:(int -> 'a) -> 'a array
val make_matrix : dimx:int -> dimy:int -> 'a -> 'a array array
val create_matrix : dimx:int -> dimy:int -> 'a -> 'a array array
val append : 'a array -> 'a array -> 'a array
val concat : 'a array list -> 'a array
val sub : 'a array -> pos:int -> len:int -> 'a array
val copy : 'a array -> 'a array
val fill : 'a array -> pos:int -> len:int -> 'a -> unit
val blit :
  src:'a array ->
    src_pos:int -> dst:'a array -> dst_pos:int -> len:int -> unit
val to_list : 'a array -> 'a list
val of_list : 'a list -> 'a array
val iter : f:('a -> unit) -> 'a array -> unit
val map : f:('a -> 'b) -> 'a array -> 'b array
val iteri : f:(int -> 'a -> unit) -> 'a array -> unit
val mapi : f:(int -> 'a -> 'b) -> 'a array -> 'b array
val fold_left : f:('a -> 'b -> 'a) -> init:'a -> 'b array -> 'a
val fold_right : f:('a -> 'b -> 'b) -> 'a array -> init:'b -> 'b
val sort : cmp:('a -> 'a -> int) -> 'a array -> unit
val stable_sort : cmp:('a -> 'a -> int) -> 'a array -> unit
val fast_sort : cmp:('a -> 'a -> int) -> 'a array -> unit
external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
external unsafe_set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"
