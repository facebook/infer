type 'a t = 'a list =
  | [] 
  | (::) of 'a * 'a list 
val length : 'a list -> int
val hd : 'a list -> 'a
val compare_lengths : 'a list -> 'b list -> int
val compare_length_with : 'a list -> len:int -> int
val cons : 'a -> 'a list -> 'a list
val tl : 'a list -> 'a list
val nth : 'a list -> int -> 'a
val nth_opt : 'a list -> int -> 'a option
val rev : 'a list -> 'a list
val init : len:int -> f:(int -> 'a) -> 'a list
val append : 'a list -> 'a list -> 'a list
val rev_append : 'a list -> 'a list -> 'a list
val concat : 'a list list -> 'a list
val flatten : 'a list list -> 'a list
val iter : f:('a -> unit) -> 'a list -> unit
val iteri : f:(int -> 'a -> unit) -> 'a list -> unit
val map : f:('a -> 'b) -> 'a list -> 'b list
val mapi : f:(int -> 'a -> 'b) -> 'a list -> 'b list
val rev_map : f:('a -> 'b) -> 'a list -> 'b list
val filter_map : f:('a -> 'b option) -> 'a list -> 'b list
val concat_map : f:('a -> 'b list) -> 'a list -> 'b list
val fold_left_map :
  f:('a -> 'b -> ('a * 'c)) -> init:'a -> 'b list -> ('a * 'c list)
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
val find_map : f:('a -> 'b option) -> 'a list -> 'b option
val filter : f:('a -> bool) -> 'a list -> 'a list
val find_all : f:('a -> bool) -> 'a list -> 'a list
val filteri : f:(int -> 'a -> bool) -> 'a list -> 'a list
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
val to_seq : 'a list -> 'a Seq.t
val of_seq : 'a Seq.t -> 'a list
