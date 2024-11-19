module Array :
sig
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
  val fold_right : f:('b -> 'a -> 'a) -> 'b array -> init:'a -> 'a
  val iter2 : f:('a -> 'b -> unit) -> 'a array -> 'b array -> unit
  val map2 : f:('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
  val exists : f:('a -> bool) -> 'a array -> bool
  val for_all : f:('a -> bool) -> 'a array -> bool
  val mem : 'a -> set:'a array -> bool
  val memq : 'a -> set:'a array -> bool
  external create_float : int -> float array = "caml_make_float_vect"
  val make_float : int -> float array
  val sort : cmp:('a -> 'a -> int) -> 'a array -> unit
  val stable_sort : cmp:('a -> 'a -> int) -> 'a array -> unit
  val fast_sort : cmp:('a -> 'a -> int) -> 'a array -> unit
  external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
  external unsafe_set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"
end
module Bytes :
sig
  external length : bytes -> int = "%bytes_length"
  external get : bytes -> int -> char = "%bytes_safe_get"
  external set : bytes -> int -> char -> unit = "%bytes_safe_set"
  external create : int -> bytes = "caml_create_bytes"
  val make : int -> char -> bytes
  val init : int -> f:(int -> char) -> bytes
  val empty : bytes
  val copy : bytes -> bytes
  val of_string : string -> bytes
  val to_string : bytes -> string
  val sub : bytes -> pos:int -> len:int -> bytes
  val sub_string : bytes -> int -> int -> string
  val extend : bytes -> left:int -> right:int -> bytes
  val fill : bytes -> pos:int -> len:int -> char -> unit
  val blit :
    src:bytes -> src_pos:int -> dst:bytes -> dst_pos:int -> len:int -> unit
  val blit_string :
    src:string -> src_pos:int -> dst:bytes -> dst_pos:int -> len:int -> unit
  val concat : sep:bytes -> bytes list -> bytes
  val cat : bytes -> bytes -> bytes
  val iter : f:(char -> unit) -> bytes -> unit
  val iteri : f:(int -> char -> unit) -> bytes -> unit
  val map : f:(char -> char) -> bytes -> bytes
  val mapi : f:(int -> char -> char) -> bytes -> bytes
  val trim : bytes -> bytes
  val escaped : bytes -> bytes
  val index : bytes -> char -> int
  val index_opt : bytes -> char -> int option
  val rindex : bytes -> char -> int
  val rindex_opt : bytes -> char -> int option
  val index_from : bytes -> int -> char -> int
  val index_from_opt : bytes -> int -> char -> int option
  val rindex_from : bytes -> int -> char -> int
  val rindex_from_opt : bytes -> int -> char -> int option
  val contains : bytes -> char -> bool
  val contains_from : bytes -> int -> char -> bool
  val rcontains_from : bytes -> int -> char -> bool
  val uppercase : bytes -> bytes
  val lowercase : bytes -> bytes
  val capitalize : bytes -> bytes
  val uncapitalize : bytes -> bytes
  val uppercase_ascii : bytes -> bytes
  val lowercase_ascii : bytes -> bytes
  val capitalize_ascii : bytes -> bytes
  val uncapitalize_ascii : bytes -> bytes
  type t = bytes
  val compare : t -> t -> int
  val equal : t -> t -> bool
  external unsafe_get : bytes -> int -> char = "%bytes_unsafe_get"
  external unsafe_set : bytes -> int -> char -> unit = "%bytes_unsafe_set"
  external unsafe_blit :
    src:bytes -> src_pos:int -> dst:bytes -> dst_pos:int -> len:int -> unit =
      "caml_blit_bytes"[@@noalloc ]
  external unsafe_fill :
    bytes -> pos:int -> len:int -> char -> unit = "caml_fill_bytes"[@@noalloc
                                                                    ]
  val unsafe_to_string : bytes -> string
  val unsafe_of_string : string -> bytes
end
module List :
sig
  val length : 'a list -> int
  val hd : 'a list -> 'a
  val compare_lengths : 'a list -> 'b list -> int
  val compare_length_with : 'a list -> len:int -> int
  val cons : 'a -> 'a list -> 'a list
  val tl : 'a list -> 'a list
  val nth : 'a list -> int -> 'a
  val nth_opt : 'a list -> int -> 'a option
  val rev : 'a list -> 'a list
  val append : 'a list -> 'a list -> 'a list
  val rev_append : 'a list -> 'a list -> 'a list
  val concat : 'a list list -> 'a list
  val flatten : 'a list list -> 'a list
  val iter : f:('a -> unit) -> 'a list -> unit
  val iteri : f:(int -> 'a -> unit) -> 'a list -> unit
  val map : f:('a -> 'b) -> 'a list -> 'b list
  val mapi : f:(int -> 'a -> 'b) -> 'a list -> 'b list
  val rev_map : f:('a -> 'b) -> 'a list -> 'b list
  val fold_left : f:('a -> 'b -> 'a) -> init:'a -> 'b list -> 'a
  val fold_right : f:('a -> 'b -> 'b) -> 'a list -> init:'b -> 'b
  val iter2 : f:('a -> 'b -> unit) -> 'a list -> 'b list -> unit
  val map2 : f:('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val rev_map2 : f:('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val fold_left2 :
    f:('a -> 'b -> 'c -> 'a) -> init:'a -> 'b list -> 'c list -> 'a
  val fold_right2 :
    f:('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> init:'c -> 'c
  val for_all : f:('a -> bool) -> 'a list -> bool
  val exists : f:('a -> bool) -> 'a list -> bool
  val for_all2 : f:('a -> 'b -> bool) -> 'a list -> 'b list -> bool
  val exists2 : f:('a -> 'b -> bool) -> 'a list -> 'b list -> bool
  val mem : 'a -> set:'a list -> bool
  val memq : 'a -> set:'a list -> bool
  val find : f:('a -> bool) -> 'a list -> 'a
  val find_opt : f:('a -> bool) -> 'a list -> 'a option
  val filter : f:('a -> bool) -> 'a list -> 'a list
  val find_all : f:('a -> bool) -> 'a list -> 'a list
  val partition : f:('a -> bool) -> 'a list -> ('a list * 'a list)
  val assoc : 'a -> ('a * 'b) list -> 'b
  val assoc_opt : 'a -> ('a * 'b) list -> 'b option
  val assq : 'a -> ('a * 'b) list -> 'b
  val assq_opt : 'a -> ('a * 'b) list -> 'b option
  val mem_assoc : 'a -> map:('a * 'b) list -> bool
  val mem_assq : 'a -> map:('a * 'b) list -> bool
  val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
  val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
  val split : ('a * 'b) list -> ('a list * 'b list)
  val combine : 'a list -> 'b list -> ('a * 'b) list
  val sort : cmp:('a -> 'a -> int) -> 'a list -> 'a list
  val stable_sort : cmp:('a -> 'a -> int) -> 'a list -> 'a list
  val fast_sort : cmp:('a -> 'a -> int) -> 'a list -> 'a list
  val sort_uniq : cmp:('a -> 'a -> int) -> 'a list -> 'a list
  val merge : cmp:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
end
module String :
sig
  external length : string -> int = "%string_length"
  external get : string -> int -> char = "%string_safe_get"
  external set : bytes -> int -> char -> unit = "%string_safe_set"
  external create : int -> bytes = "caml_create_string"
  val make : int -> char -> string
  val init : int -> f:(int -> char) -> string
  val copy : string -> string
  val sub : string -> pos:int -> len:int -> string
  val fill : bytes -> pos:int -> len:int -> char -> unit
  val blit :
    src:string -> src_pos:int -> dst:bytes -> dst_pos:int -> len:int -> unit
  val concat : sep:string -> string list -> string
  val iter : f:(char -> unit) -> string -> unit
  val iteri : f:(int -> char -> unit) -> string -> unit
  val map : f:(char -> char) -> string -> string
  val mapi : f:(int -> char -> char) -> string -> string
  val trim : string -> string
  val escaped : string -> string
  val index : string -> char -> int
  val index_opt : string -> char -> int option
  val rindex : string -> char -> int
  val rindex_opt : string -> char -> int option
  val index_from : string -> int -> char -> int
  val index_from_opt : string -> int -> char -> int option
  val rindex_from : string -> int -> char -> int
  val rindex_from_opt : string -> int -> char -> int option
  val contains : string -> char -> bool
  val contains_from : string -> int -> char -> bool
  val rcontains_from : string -> int -> char -> bool
  val uppercase : string -> string
  val lowercase : string -> string
  val capitalize : string -> string
  val uncapitalize : string -> string
  val uppercase_ascii : string -> string
  val lowercase_ascii : string -> string
  val capitalize_ascii : string -> string
  val uncapitalize_ascii : string -> string
  type t = string
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val split_on_char : sep:char -> string -> string list
  external unsafe_get : string -> int -> char = "%string_unsafe_get"
  external unsafe_set : bytes -> int -> char -> unit = "%string_unsafe_set"
  external unsafe_blit :
    src:string -> src_pos:int -> dst:bytes -> dst_pos:int -> len:int -> unit
      = "caml_blit_string"[@@noalloc ]
  external unsafe_fill :
    bytes -> pos:int -> len:int -> char -> unit = "caml_fill_string"[@@noalloc
                                                                    ]
end
