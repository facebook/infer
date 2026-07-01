type status =
  | Passed
  | Failed of string
  | Disabled of string

val add_test: title:string -> (unit -> status) -> unit

val use_version: (int option * int option) ref

val enable_only_on_unix: ('a -> status) -> 'a -> status

val launch_tests : unit -> unit

val main: unit -> unit
