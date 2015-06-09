(*
* Copyright (c) 2013 - Facebook.
* All rights reserved.
*)

(** This module handles buckets of memory leaks in Objective-C *)

val objc_arc_flag : string

val init_buckets : string -> unit

(* Returns whether a memory leak should be raised. If objc_ml_buckets is not there, *)
(* then raise all memory leaks. *)
(* If cf is passed, then check leaks from Core Foundation. *)
(* If arc is passed, check leaks from code that compiles with arc*)
(* If no arc is passed check the leaks from code that compiles without arc *)
val should_raise_leak : Sil.typ -> string option

