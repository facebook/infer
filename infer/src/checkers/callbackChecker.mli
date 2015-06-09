(*
* Copyright (c) 2013 - Facebook. All rights reserved.
*)

(** Make sure callbacks are always unregistered. drive the point home by reporting possible NPE's *)

(** return the set of instance fields that are assigned to a null literal in [procdesc] *)
val get_fields_nullified : Cfg.Procdesc.t -> Ident.FieldSet.t

val callback_checker_main : Callbacks.proc_callback_t
