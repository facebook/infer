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
val set_uncaught_exception_handler : (exn -> raw_backtrace -> unit) -> unit
type backtrace_slot
val backtrace_slots : raw_backtrace -> backtrace_slot array option
type location =
  {
  filename: string ;
  line_number: int ;
  start_char: int ;
  end_char: int }
module Slot :
sig
  type t = backtrace_slot
  val is_raise : t -> bool
  val location : t -> location option
  val format : int -> t -> string option
end
type raw_backtrace_slot
val raw_backtrace_length : raw_backtrace -> int
val get_raw_backtrace_slot : raw_backtrace -> int -> raw_backtrace_slot
val convert_raw_backtrace_slot : raw_backtrace_slot -> backtrace_slot
val exn_slot_id : exn -> int
val exn_slot_name : exn -> string
