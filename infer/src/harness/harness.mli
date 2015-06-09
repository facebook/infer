(*
* Copyright (c) 2013 - Facebook. All rights reserved.
*)

(** Automatically create a harness method to exercise code under test *)

(** Generate a harness method for exe_env and add it to the execution environment *)
val create_harness : DB.source_file Procname.Map.t -> Sil.tenv -> unit
