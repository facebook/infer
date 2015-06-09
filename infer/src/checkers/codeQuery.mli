(*
* Copyright (c) 2013 - Facebook. All rights reserved.
*)

(** Module for code queries. *)

val code_query_callback : Callbacks.proc_callback_t

val query : string option ref