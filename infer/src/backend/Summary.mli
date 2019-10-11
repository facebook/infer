(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Procedure summaries: the results of the capture and all the analysis for a single procedure,
   plus some statistics *)

module Stats : sig
  (** Execution statistics *)
  type t

  val add_visited : t -> int -> unit

  val is_visited : t -> int -> bool

  val nb_visited : t -> int

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
  ; mutable sessions: int  (** Session number: how many nodes went through symbolic execution *)
  ; stats: Stats.t
  ; status: Status.t
  ; proc_desc: Procdesc.t
  ; err_log: Errlog.t
  ; mutable callee_pnames: Typ.Procname.Set.t }

val poly_fields : t PolyFields.t

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

val get_status : t -> Status.t
(** Return the status (active v.s. inactive) of a procedure summary *)

val pp_html : SourceFile.t -> Format.formatter -> t -> unit
(** Print the summary in html format *)

val pp_text : Format.formatter -> t -> unit
(** Print the summary in text format *)

module OnDisk : sig
  val has_model : Typ.Procname.t -> bool
  (** Check if a summary for a given procedure exists in the models directory *)

  val clear_cache : unit -> unit
  (** Remove all the elements from the cache of summaries *)

  val remove_from_cache : Typ.Procname.t -> unit
  (** Remove an element from the cache of summaries. Contrast to reset which re-initializes a summary
    keeping the same Procdesc and updates the cache accordingly. *)

  val get : Typ.Procname.t -> t option
  (** Return the summary option for the procedure name *)

  val reset : Procdesc.t -> t
  (** Reset a summary rebuilding the dependents and preserving the proc attributes if present. *)

  val specs_filename_of_procname : Typ.Procname.t -> DB.filename
  (** Return the path to the .specs file for the given procedure in the current results directory *)

  val load_from_file : DB.filename -> t option
  (** Load procedure summary from the given file *)

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

  val reset_all : filter:Filtering.procedures_filter -> unit -> unit

  val dummy : t
  (** dummy summary for testing *)
end
