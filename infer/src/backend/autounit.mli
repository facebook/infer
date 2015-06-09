(*
* Copyright (c) 2009-2013 Monoidics ltd.
* Copyright (c) 2013- Facebook.
* All rights reserved.
*)

(** Generate unit tests automatically from specs *)

(** type of generated code *)
type code

(** pretty print generated code *)
val pp_code : Format.formatter -> code -> unit

(** generate a unit test form a spec *)
val genunit : string -> Procname.t -> int -> (string * Sil.typ) list -> Prop.normal Specs.spec -> code

(** generate code for a main calling all the unit test functions passed as argument *)
val genmain : (Procname.t * int) list -> code