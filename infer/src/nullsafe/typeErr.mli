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
      { base_proc_name: Typ.Procname.t
      ; overridden_proc_name: Typ.Procname.t
      ; inconsistent_subclass_type: inconsistent_subclass_type }
  | Field_not_initialized of Typ.Fieldname.t * Typ.Procname.t
  | Over_annotation of over_annotation_type
  | Nullable_dereference of
      { nullable_object_descr: string option
      ; dereference_type: dereference_type
      ; origin_descr: origin_descr }
  | Bad_assignment of {rhs_origin_descr: origin_descr; assignment_type: assignment_type}
[@@deriving compare]

and inconsistent_subclass_type =
  | InconsistentParam of {param_description: string; param_position: int}
  | InconsistentReturn

and over_annotation_type =
  | FieldOverAnnotedAsNullable of Typ.Fieldname.t
  | ReturnOverAnnotatedAsNullable of Typ.Procname.t
      (** Return value of a method can be made non-nullable *)

and assignment_type =
  | PassingParamToAFunction of
      { param_description: string
      ; param_position: int
      ; function_procname: Typ.Procname.t }
  | AssigningToAField of Typ.Fieldname.t
  | ReturningFromAFunction of Typ.Procname.t

and dereference_type =
  | MethodCall of Typ.Procname.t  (** nullable_object.some_method() *)
  | AccessToField of Typ.Fieldname.t  (** nullable_object.some_field *)
  | AccessByIndex of {index_desc: string}  (** nullable_array[some_index] *)
  | ArrayLengthAccess  (** nullable_array.length *)

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
