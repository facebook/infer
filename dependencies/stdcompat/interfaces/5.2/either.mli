type ('a, 'b) t =
  | Left of 'a 
  | Right of 'b 
val left : 'a -> ('a, 'b) t
val right : 'b -> ('a, 'b) t
val is_left : ('a, 'b) t -> bool
val is_right : ('a, 'b) t -> bool
val find_left : ('a, 'b) t -> 'a option
val find_right : ('a, 'b) t -> 'b option
val map_left : ('a1 -> 'a2) -> ('a1, 'b) t -> ('a2, 'b) t
val map_right : ('b1 -> 'b2) -> ('a, 'b1) t -> ('a, 'b2) t
val map :
  left:('a1 -> 'a2) -> right:('b1 -> 'b2) -> ('a1, 'b1) t -> ('a2, 'b2) t
val fold : left:('a -> 'c) -> right:('b -> 'c) -> ('a, 'b) t -> 'c
val iter : left:('a -> unit) -> right:('b -> unit) -> ('a, 'b) t -> unit
val for_all : left:('a -> bool) -> right:('b -> bool) -> ('a, 'b) t -> bool
val equal :
  left:('a -> 'a -> bool) ->
    right:('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
val compare :
  left:('a -> 'a -> int) ->
    right:('b -> 'b -> int) -> ('a, 'b) t -> ('a, 'b) t -> int
