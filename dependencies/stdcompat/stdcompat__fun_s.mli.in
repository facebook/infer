module type S = sig
exception Finally_raised of exn 
(** Alias for {!Fun.Finally_raised} *)

val compose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
(** @since 5.2.0: val compose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c *)

external id : 'a -> 'a = "%identity"
(** @since 4.08.0: external id : 'a -> 'a = "%identity" *)

val const : 'a -> 'b -> 'a
(** @since 4.08.0: val const : 'a -> 'b -> 'a *)

val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
(** @since 4.08.0: val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c *)

val negate : ('a -> bool) -> 'a -> bool
(** @since 4.08.0: val negate : ('a -> bool) -> 'a -> bool *)

val protect : finally:(unit -> unit) -> (unit -> 'a) -> 'a
(** @since 4.08.0: val protect : finally:(unit -> unit) -> (unit -> 'a) -> 'a *)

end
