type 'a t = 'a option =
  | None 
  | Some of 'a 
val none : 'a option
val some : 'a -> 'a option
val value : 'a option -> default:'a -> 'a
val get : 'a option -> 'a
val bind : 'a option -> ('a -> 'b option) -> 'b option
val join : 'a option option -> 'a option
val map : ('a -> 'b) -> 'a option -> 'b option
val fold : none:'a -> some:('b -> 'a) -> 'b option -> 'a
val iter : ('a -> unit) -> 'a option -> unit
val is_none : 'a option -> bool
val is_some : 'a option -> bool
val equal : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool
val compare : ('a -> 'a -> int) -> 'a option -> 'a option -> int
val to_result : none:'e -> 'a option -> ('a, 'e) result
val to_list : 'a option -> 'a list
val to_seq : 'a option -> 'a Seq.t
