(*
 * Copyright (c) 2014-present, Facebook, Inc.
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

type parameter_not_nullable =
  AnnotatedSignature.annotation
  * string
  * (* description *)
    int
  * (* parameter number *)
    Typ.Procname.t
  * Location.t
  * (* callee location *)
    origin_descr

(** Instance of an error *)
type err_instance =
  | Condition_redundant of (bool * string option * bool)
  | Inconsistent_subclass_return_annotation of Typ.Procname.t * Typ.Procname.t
  | Inconsistent_subclass_parameter_annotation of string * int * Typ.Procname.t * Typ.Procname.t
  | Field_not_initialized of Typ.Fieldname.t * Typ.Procname.t
  | Field_not_mutable of Typ.Fieldname.t * origin_descr
  | Field_annotation_inconsistent of AnnotatedSignature.annotation * Typ.Fieldname.t * origin_descr
  | Field_over_annotated of Typ.Fieldname.t * Typ.Procname.t
  | Null_field_access of string option * Typ.Fieldname.t * origin_descr * bool
  | Call_receiver_annotation_inconsistent of
      AnnotatedSignature.annotation * string option * Typ.Procname.t * origin_descr
  | Parameter_annotation_inconsistent of parameter_not_nullable
  | Return_annotation_inconsistent of AnnotatedSignature.annotation * Typ.Procname.t * origin_descr
  | Return_over_annotated of Typ.Procname.t

val node_reset_forall : Procdesc.Node.t -> unit

type st_report_error =
     Typ.Procname.t
  -> Procdesc.t
  -> IssueType.t
  -> Location.t
  -> ?field_name:Typ.Fieldname.t option
  -> ?origin_loc:Location.t option
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
