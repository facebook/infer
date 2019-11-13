(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(** Assignment rule should be checked when a value is assigned to a location.
    Assignment can be explicit (lhs = rhs) or implicit (e.g. returning from a function).
    This rule checks if null can be passed to a place that does not expect null.
 *)

type violation [@@deriving compare]

val check :
  is_strict_mode:bool -> lhs:Nullability.t -> rhs:Nullability.t -> (unit, violation) result

type assignment_type =
  | PassingParamToFunction of
      {param_description: string; param_position: int; function_procname: Typ.Procname.t}
  | AssigningToField of Typ.Fieldname.t
  | ReturningFromFunction of Typ.Procname.t
[@@deriving compare]

val violation_description : violation -> assignment_type -> rhs_origin_descr:string -> string
