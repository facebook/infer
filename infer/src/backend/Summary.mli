(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Procedure summaries: the results of the capture and all the analysis for a single procedure,
   plus some statistics *)

module Stats : sig
  (** Execution statistics *)
  type t

  val add_visited_fp : t -> int -> unit

  val add_visited_re : t -> int -> unit

  val is_visited_fp : t -> int -> bool

  val is_visited_re : t -> int -> bool

  val nb_visited_re : t -> int

  val update : ?add_symops:int -> ?failure_kind:SymOp.failure_kind -> t -> t

  val failure_kind : t -> SymOp.failure_kind option

  val failure_kind_to_string : t -> string

  val symops : t -> int
end

module Status : sig
  (** Analysis status of the procedure *)
  type t

  val is_analyzed : t -> bool

  val to_string : t -> string
end

(** summary of a procedure name *)
type t =
  { payloads: Payloads.t
  ; sessions: int ref  (** Session number: how many nodes went through symbolic execution *)
  ; stats: Stats.t
  ; status: Status.t
  ; proc_desc: Procdesc.t }

val dummy : t
(** dummy summary for testing *)

val add : Typ.Procname.t -> t -> unit
(** Add the summary to the table for the given function *)

val has_model : Typ.Procname.t -> bool
(** Check if a summary for a given procedure exists in the models directory *)

val clear_cache : unit -> unit
(** remove all the elements from the cache of summaries *)

val get : Typ.Procname.t -> t option
(** Return the summary option for the procedure name *)

val get_proc_name : t -> Typ.Procname.t
(** Get the procedure name *)

val get_proc_desc : t -> Procdesc.t

val get_attributes : t -> ProcAttributes.t
(** Get the attributes of the procedure. *)

val get_formals : t -> (Mangled.t * Typ.t) list
(** Get the formal parameters of the procedure *)

val get_err_log : t -> Errlog.t

val get_loc : t -> Location.t

val get_signature : t -> string
(** Return the signature of a procedure declaration as a string *)

val get_unsafe : Typ.Procname.t -> t
(** @deprecated Return the summary for the procedure name. Raises an exception when not found. *)

val get_status : t -> Status.t
(** Return the status (active v.s. inactive) of a procedure summary *)

val reset : Procdesc.t -> t
(** Reset a summary rebuilding the dependents and preserving the proc attributes if present. *)

val load_from_file : DB.filename -> t option

val pp_html : SourceFile.t -> Pp.color -> Format.formatter -> t -> unit
(** Print the summary in html format *)

val pp_text : Format.formatter -> t -> unit
(** Print the summary in text format *)

val pdesc_resolve_attributes : Procdesc.t -> ProcAttributes.t
(** Like proc_resolve_attributes but start from a proc_desc. *)

val proc_resolve_attributes : Typ.Procname.t -> ProcAttributes.t option
(** Try to find the attributes for a defined proc.
    First look at specs (to get attributes computed by analysis)
    then look at the attributes table.
    If no attributes can be found, return None.
*)

val proc_is_library : ProcAttributes.t -> bool
(** Check if the procedure is from a library:
    It's not defined, and there is no spec file for it. *)

val store : t -> unit
(** Save summary for the procedure into the spec database *)
