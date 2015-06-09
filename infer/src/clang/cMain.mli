(*
* Copyright (c) 2013 - Facebook.
* All rights reserved.
*)

(** Main module of InferClang. Take as input AST files produced by clang during compilation *)
(** and their corresponding C/C++/ObjectiveC source files. *)
(** Parse the arguments, parse and validate the input AST into a data structure *)
(** and translates it into a cfg. *)


val do_run : string -> string option -> unit
