(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for Type Error messages. *)

module type InstrRefT = sig
  type t [@@deriving compare]

  val equal : t -> t -> bool

  type generator

  val create_generator : Procdesc.Node.t -> generator

  val gen : generator -> t

  val get_node : t -> Procdesc.Node.t

  val hash : t -> int

  val replace_node : t -> Procdesc.Node.t -> t
end

(* InstrRefT *)
module InstrRef : InstrRefT

type origin_descr = string * Location.t option * AnnotatedSignature.t option

(* callee signature *)

(** Instance of an error *)
type err_instance =
  | Condition_redundant of (bool * string option)
  | Inconsistent_subclass of
      { inheritance_violation: InheritanceRule.violation
      ; violation_type: InheritanceRule.violation_type
      ; base_proc_name: Typ.Procname.t
      ; overridden_proc_name: Typ.Procname.t }
  | Field_not_initialized of Typ.Fieldname.t * Typ.Procname.t
  | Over_annotation of
      { over_annotated_violation: OverAnnotatedRule.violation
      ; violation_type: OverAnnotatedRule.violation_type }
  | Nullable_dereference of
      { dereference_violation: DereferenceRule.violation
      ; dereference_type: DereferenceRule.dereference_type
      ; nullable_object_descr: string option
      ; origin_descr: origin_descr }
  | Bad_assignment of
      { assignment_violation: AssignmentRule.violation
      ; assignment_type: AssignmentRule.assignment_type
      ; rhs_origin_descr: origin_descr }
[@@deriving compare]

val node_reset_forall : Procdesc.Node.t -> unit

type st_report_error =
     Typ.Procname.t
  -> Procdesc.t
  -> IssueType.t
  -> Location.t
  -> ?field_name:Typ.Fieldname.t option
  -> ?exception_kind:(IssueType.t -> Localise.error_desc -> exn)
  -> ?severity:Exceptions.severity
  -> string
  -> unit

val report_error :
     Tenv.t
  -> st_report_error
  -> (Procdesc.Node.t -> Procdesc.Node.t)
  -> err_instance
  -> InstrRef.t option
  -> Location.t
  -> Procdesc.t
  -> unit

val report_forall_checks_and_reset : Tenv.t -> st_report_error -> Procdesc.t -> unit

val reset : unit -> unit
