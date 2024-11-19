val to_string : exn -> string
val print : ('a -> 'b) -> 'a -> 'b
val catch : ('a -> 'b) -> 'a -> 'b
val print_backtrace : out_channel -> unit
val get_backtrace : unit -> string
val record_backtrace : bool -> unit
val backtrace_status : unit -> bool
val register_printer : (exn -> string option) -> unit
type raw_backtrace
val get_raw_backtrace : unit -> raw_backtrace
val print_raw_backtrace : out_channel -> raw_backtrace -> unit
val raw_backtrace_to_string : raw_backtrace -> string
val get_callstack : int -> raw_backtrace
