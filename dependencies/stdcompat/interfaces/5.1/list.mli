type 'a t = 'a list =
  | [] 
  | (::) of 'a * 'a list 
val length : 'a list -> int
val compare_lengths : 'a list -> 'b list -> int
val compare_length_with : 'a list -> int -> int
val is_empty : 'a list -> bool
val cons : 'a -> 'a list -> 'a list
val hd : 'a list -> 'a
val tl : 'a list -> 'a list
val nth : 'a list -> int -> 'a
val nth_opt : 'a list -> int -> 'a option
val rev : 'a list -> 'a list
val init : int -> (int -> 'a) -> 'a list
val append : 'a list -> 'a list -> 'a list
val rev_append : 'a list -> 'a list -> 'a list
val concat : 'a list list -> 'a list
val flatten : 'a list list -> 'a list
val equal : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
val compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int
val iter : ('a -> unit) -> 'a list -> unit
val iteri : (int -> 'a -> unit) -> 'a list -> unit
val map : ('a -> 'b) -> 'a list -> 'b list
val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
val rev_map : ('a -> 'b) -> 'a list -> 'b list
val filter_map : ('a -> 'b option) -> 'a list -> 'b list
val concat_map : ('a -> 'b list) -> 'a list -> 'b list
val fold_left_map :
  ('acc -> 'a -> ('acc * 'b)) -> 'acc -> 'a list -> ('acc * 'b list)
val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc
val fold_right : ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc
val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val fold_left2 :
  ('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a list -> 'b list -> 'acc
val fold_right2 :
  ('a -> 'b -> 'acc -> 'acc) -> 'a list -> 'b list -> 'acc -> 'acc
val for_all : ('a -> bool) -> 'a list -> bool
val exists : ('a -> bool) -> 'a list -> bool
val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
val mem : 'a -> 'a list -> bool
val memq : 'a -> 'a list -> bool
val find : ('a -> bool) -> 'a list -> 'a
val find_opt : ('a -> bool) -> 'a list -> 'a option
val find_index : ('a -> bool) -> 'a list -> int option
val find_map : ('a -> 'b option) -> 'a list -> 'b option
val find_mapi : (int -> 'a -> 'b option) -> 'a list -> 'b option
val filter : ('a -> bool) -> 'a list -> 'a list
val find_all : ('a -> bool) -> 'a list -> 'a list
val filteri : (int -> 'a -> bool) -> 'a list -> 'a list
val partition : ('a -> bool) -> 'a list -> ('a list * 'a list)
val partition_map :
  ('a -> ('b, 'c) Either.t) -> 'a list -> ('b list * 'c list)
val assoc : 'a -> ('a * 'b) list -> 'b
val assoc_opt : 'a -> ('a * 'b) list -> 'b option
val assq : 'a -> ('a * 'b) list -> 'b
val assq_opt : 'a -> ('a * 'b) list -> 'b option
val mem_assoc : 'a -> ('a * 'b) list -> bool
val mem_assq : 'a -> ('a * 'b) list -> bool
val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
val split : ('a * 'b) list -> ('a list * 'b list)
val combine : 'a list -> 'b list -> ('a * 'b) list
val sort : ('a -> 'a -> int) -> 'a list -> 'a list
val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list
val sort_uniq : ('a -> 'a -> int) -> 'a list -> 'a list
val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
val to_seq : 'a list -> 'a Seq.t
val of_seq : 'a Seq.t -> 'a list
