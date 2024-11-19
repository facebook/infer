val to_string : exn -> string
val print : ('a -> 'b) -> 'a -> 'b
val catch : ('a -> 'b) -> 'a -> 'b
val print_backtrace : out_channel -> unit
val get_backtrace : unit -> string
val record_backtrace : bool -> unit
val backtrace_status : unit -> bool
val register_printer : (exn -> string option) -> unit
