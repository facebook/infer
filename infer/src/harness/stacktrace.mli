(*
* Copyright (c) 2013 - Facebook. All rights reserved.
*)

(** Module for parsing stack traces and using them to guide Infer analysis *)

open Utils

type stack_trace

(** create an Infer-readable representation of a stack trace given its raw text *)
val parse_stack_trace : string -> Exe_env.t -> stack_trace
