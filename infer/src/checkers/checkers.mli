(*
* Copyright (c) 2013 - Facebook. All rights reserved.
*)

(** Module for user-defined checkers. *)


(** State that persists in the .specs files. *)
module ST : sig
(** Add a key/value pair. *)
  val pname_add : Procname.t -> string -> string -> unit
  (** Find the value associated to the key. Raise Not_found if it does not exist. *)
  val pname_find: Procname.t -> string -> string

  (** Report an error. *)
  val report_error:
  Procname.t ->
  Cfg.Procdesc.t ->
  string ->
  Sil.location ->
  ?advice: string option ->
  ?field_name: Ident.fieldname option ->
  ?origin_loc: Sil.location option ->
  ?exception_kind: (string -> Localise.error_desc -> exn) ->
  ?always_report: bool ->
  string ->
  unit

  (** Store the summary to a .specs file. *)
  val store_summary : Procname.t -> unit

end (* ST *)

module PP : sig
(** Print a range of lines of the source file in [loc], including [nbefore] lines before loc
and [nafter] lines after [loc] *)
  val pp_loc_range : Printer.LineReader.t -> int -> int -> Format.formatter -> Sil.location -> unit
end (* PP *)

val callback_check_access : Callbacks.proc_callback_t
val callback_check_cluster_access : Callbacks.cluster_callback_t
val callback_monitor_nullcheck : Callbacks.proc_callback_t
val callback_test_state : Callbacks.proc_callback_t
val callback_checkVisibleForTesting : Callbacks.proc_callback_t
val callback_check_write_to_parcel : Callbacks.proc_callback_t
val callback_find_deserialization : Callbacks.proc_callback_t
val callback_check_field_access : Callbacks.proc_callback_t
val callback_print_c_method_calls : Callbacks.proc_callback_t
