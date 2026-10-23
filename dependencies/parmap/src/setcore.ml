(* uses the native affinity interface to
   declare that the current process should be
   attached to core number n *)

external numcores: unit -> int = "numcores"
external setcore: int -> unit = "setcore"
