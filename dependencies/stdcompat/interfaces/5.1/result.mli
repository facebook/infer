type ('a, 'e) t = ('a, 'e) result =
  | Ok of 'a 
  | Error of 'e 
val ok : 'a -> ('a, 'e) result
val error : 'e -> ('a, 'e) result
val value : ('a, 'e) result -> default:'a -> 'a
val get_ok : ('a, 'e) result -> 'a
val get_error : ('a, 'e) result -> 'e
val bind : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
val join : (('a, 'e) result, 'e) result -> ('a, 'e) result
val map : ('a -> 'b) -> ('a, 'e) result -> ('b, 'e) result
val map_error : ('e -> 'f) -> ('a, 'e) result -> ('a, 'f) result
val fold : ok:('a -> 'c) -> error:('e -> 'c) -> ('a, 'e) result -> 'c
val iter : ('a -> unit) -> ('a, 'e) result -> unit
val iter_error : ('e -> unit) -> ('a, 'e) result -> unit
val is_ok : ('a, 'e) result -> bool
val is_error : ('a, 'e) result -> bool
val equal :
  ok:('a -> 'a -> bool) ->
    error:('e -> 'e -> bool) -> ('a, 'e) result -> ('a, 'e) result -> bool
val compare :
  ok:('a -> 'a -> int) ->
    error:('e -> 'e -> int) -> ('a, 'e) result -> ('a, 'e) result -> int
val to_option : ('a, 'e) result -> 'a option
val to_list : ('a, 'e) result -> 'a list
val to_seq : ('a, 'e) result -> 'a Seq.t
