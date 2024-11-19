type 'a t = unit -> 'a node
and 'a node =
  | Nil 
  | Cons of 'a * 'a t 
val is_empty : 'a t -> bool
val uncons : 'a t -> ('a * 'a t) option
val length : 'a t -> int
val iter : ('a -> unit) -> 'a t -> unit
val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
val iteri : (int -> 'a -> unit) -> 'a t -> unit
val fold_lefti : ('acc -> int -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
val for_all : ('a -> bool) -> 'a t -> bool
val exists : ('a -> bool) -> 'a t -> bool
val find : ('a -> bool) -> 'a t -> 'a option
val find_index : ('a -> bool) -> 'a t -> int option
val find_map : ('a -> 'b option) -> 'a t -> 'b option
val find_mapi : (int -> 'a -> 'b option) -> 'a t -> 'b option
val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
val fold_left2 : ('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a t -> 'b t -> 'acc
val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
val equal : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
val compare : ('a -> 'b -> int) -> 'a t -> 'b t -> int
val empty : 'a t
val return : 'a -> 'a t
val cons : 'a -> 'a t -> 'a t
val init : int -> (int -> 'a) -> 'a t
val unfold : ('b -> ('a * 'b) option) -> 'b -> 'a t
val repeat : 'a -> 'a t
val forever : (unit -> 'a) -> 'a t
val cycle : 'a t -> 'a t
val iterate : ('a -> 'a) -> 'a -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
val filter : ('a -> bool) -> 'a t -> 'a t
val filter_map : ('a -> 'b option) -> 'a t -> 'b t
val scan : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b t
val take : int -> 'a t -> 'a t
val drop : int -> 'a t -> 'a t
val take_while : ('a -> bool) -> 'a t -> 'a t
val drop_while : ('a -> bool) -> 'a t -> 'a t
val group : ('a -> 'a -> bool) -> 'a t -> 'a t t
val memoize : 'a t -> 'a t
exception Forced_twice 
val once : 'a t -> 'a t
val transpose : 'a t t -> 'a t t
val append : 'a t -> 'a t -> 'a t
val concat : 'a t t -> 'a t
val flat_map : ('a -> 'b t) -> 'a t -> 'b t
val concat_map : ('a -> 'b t) -> 'a t -> 'b t
val zip : 'a t -> 'b t -> ('a * 'b) t
val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val interleave : 'a t -> 'a t -> 'a t
val sorted_merge : ('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
val product : 'a t -> 'b t -> ('a * 'b) t
val map_product : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val unzip : ('a * 'b) t -> ('a t * 'b t)
val split : ('a * 'b) t -> ('a t * 'b t)
val partition_map : ('a -> ('b, 'c) Either.t) -> 'a t -> ('b t * 'c t)
val partition : ('a -> bool) -> 'a t -> ('a t * 'a t)
val of_dispenser : (unit -> 'a option) -> 'a t
val to_dispenser : 'a t -> unit -> 'a option
val ints : int -> int t
