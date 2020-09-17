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

(* callee signature *)

(** Instance of an error *)
type err_instance =
  | Condition_redundant of
      { loc: Location.t
      ; is_always_true: bool
      ; condition_descr: string option
      ; nonnull_origin: TypeOrigin.t }
  | Inconsistent_subclass of
      { loc: Location.t
      ; inheritance_violation: InheritanceRule.violation
      ; violation_type: InheritanceRule.ReportableViolation.violation_type
      ; base_proc_name: Procname.Java.t
      ; overridden_proc_name: Procname.Java.t }
  | Field_not_initialized of {loc: Location.t; field_name: Fieldname.t}
  | Over_annotation of
      { loc: Location.t
      ; over_annotated_violation: OverAnnotatedRule.violation
      ; violation_type: OverAnnotatedRule.violation_type }
  | Nullable_dereference of
      { dereference_violation: DereferenceRule.violation
      ; dereference_location: Location.t
      ; dereference_type: DereferenceRule.ReportableViolation.dereference_type
      ; nullable_object_descr: string option }
  | Bad_assignment of
      { assignment_violation: AssignmentRule.violation
      ; assignment_location: Location.t
      ; assignment_type: AssignmentRule.ReportableViolation.assignment_type }
[@@deriving compare]

val pp_err_instance : Format.formatter -> err_instance -> unit

val node_reset_forall : Procdesc.Node.t -> unit

val register_error :
     IntraproceduralAnalysis.t
  -> (Procdesc.Node.t -> Procdesc.Node.t)
  -> err_instance
  -> nullsafe_mode:NullsafeMode.t
  -> InstrRef.t option
  -> unit
(** Register the fact that issue happened. Depending on the error and mode, this error might or
    might not be reported to the user. *)

val report_forall_issues_and_reset :
  IntraproceduralAnalysis.t -> nullsafe_mode:NullsafeMode.t -> unit
(** Report registered "forall" issues (if needed), and reset the error table *)

val is_reportable : nullsafe_mode:NullsafeMode.t -> err_instance -> bool
(** Is a given issue reportable to the user in a given mode? *)

val reset : unit -> unit

val get_errors : unit -> (err_instance * InstrRef.t option) list
