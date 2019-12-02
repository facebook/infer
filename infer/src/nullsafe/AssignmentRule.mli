(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(** Assignment rule should be checked when a value is assigned to a location. Assignment can be
    explicit (lhs = rhs) or implicit (e.g. returning from a function). This rule checks if null can
    be passed to a place that does not expect null. *)

type violation [@@deriving compare]

val check :
  is_strict_mode:bool -> lhs:Nullability.t -> rhs:Nullability.t -> (unit, violation) result

type assignment_type =
  | PassingParamToFunction of function_info
  | AssigningToField of Typ.Fieldname.t
  | ReturningFromFunction of Typ.Procname.t
[@@deriving compare]

and function_info =
  { param_signature: AnnotatedSignature.param_signature
  ; model_source: AnnotatedSignature.model_source option
  ; actual_param_expression: string
  ; param_position: int
  ; function_procname: Typ.Procname.t }

val violation_description :
     violation
  -> assignment_location:Location.t
  -> assignment_type
  -> rhs_origin:TypeOrigin.t
  -> string * IssueType.t * Location.t
(** Given context around violation, return error message together with the info where to put this
    message *)
