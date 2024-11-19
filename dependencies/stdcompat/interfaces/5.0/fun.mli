external id : 'a -> 'a = "%identity"
val const : 'a -> 'b -> 'a
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
val negate : ('a -> bool) -> 'a -> bool
val protect : finally:(unit -> unit) -> (unit -> 'a) -> 'a
exception Finally_raised of exn 
