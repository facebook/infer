type t = exn = ..
val to_string : exn -> string
val to_string_default : exn -> string
val print : ('a -> 'b) -> 'a -> 'b
val catch : ('a -> 'b) -> 'a -> 'b
val print_backtrace : out_channel -> unit
val get_backtrace : unit -> string
val record_backtrace : bool -> unit
val backtrace_status : unit -> bool
val register_printer : (exn -> string option) -> unit
val use_printers : exn -> string option
type raw_backtrace
type raw_backtrace_entry = private int
val raw_backtrace_entries : raw_backtrace -> raw_backtrace_entry array
val get_raw_backtrace : unit -> raw_backtrace
val print_raw_backtrace : out_channel -> raw_backtrace -> unit
val raw_backtrace_to_string : raw_backtrace -> string
external raise_with_backtrace :
  exn -> raw_backtrace -> 'a = "%raise_with_backtrace"
external get_callstack : int -> raw_backtrace = "caml_get_current_callstack"
val default_uncaught_exception_handler : exn -> raw_backtrace -> unit
val set_uncaught_exception_handler : (exn -> raw_backtrace -> unit) -> unit
type backtrace_slot
val backtrace_slots : raw_backtrace -> backtrace_slot array option
val backtrace_slots_of_raw_entry :
  raw_backtrace_entry -> backtrace_slot array option
type location =
  {
  filename: string ;
  line_number: int ;
  start_char: int ;
  end_char: int ;
  end_line: int ;
  end_col: int }
module Slot :
sig
  type t = backtrace_slot
  val is_raise : t -> bool
  val is_inline : t -> bool
  val location : t -> location option
  val name : t -> string option
  val format : int -> t -> string option
end
type raw_backtrace_slot
val raw_backtrace_length : raw_backtrace -> int
val get_raw_backtrace_slot : raw_backtrace -> int -> raw_backtrace_slot
val convert_raw_backtrace_slot : raw_backtrace_slot -> backtrace_slot
val get_raw_backtrace_next_slot :
  raw_backtrace_slot -> raw_backtrace_slot option
val exn_slot_id : exn -> int
val exn_slot_name : exn -> string
val string_of_extension_constructor : Obj.t -> string
