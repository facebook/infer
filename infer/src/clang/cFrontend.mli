(*
* Copyright (c) 2013 - Facebook.
* All rights reserved.
*)

(** Translate one file into a cfg. Create a tenv, cg and cfg file for a source file    *)
(** given its ast in json format. Translate the json file into a cfg by adding all     *)
(** the type and class declarations to the tenv, adding all the functions and methods  *)
(** declarations as procdescs to the cfg, and adding the control flow graph of all the *)
(** code of those functions and methods to the cfg   *)


val do_source_file : DB.source_file -> Clang_ast_t.decl -> unit