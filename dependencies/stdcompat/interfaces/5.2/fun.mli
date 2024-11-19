external id : 'a -> 'a = "%identity"
val const : 'a -> 'b -> 'a
val compose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
val negate : ('a -> bool) -> 'a -> bool
val protect : finally:(unit -> unit) -> (unit -> 'a) -> 'a
exception Finally_raised of exn 
