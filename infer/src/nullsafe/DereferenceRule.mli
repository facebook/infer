(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(** Dereference rule should be checked every type an object is dereferenced. The rule checks if the
    reference is nullable. *)

type violation [@@deriving compare]

val check : nullsafe_mode:NullsafeMode.t -> Nullability.t -> (unit, violation) result

type dereference_type =
  | MethodCall of Procname.t
  | AccessToField of Fieldname.t
  | AccessByIndex of {index_desc: string}
  | ArrayLengthAccess
[@@deriving compare]

val violation_description :
     violation
  -> dereference_location:Location.t
  -> dereference_type
  -> nullable_object_descr:string option
  -> nullable_object_origin:TypeOrigin.t
  -> string * IssueType.t * Location.t
(** Given context around violation, return error message together with the info where to put this
    message *)

val violation_severity : violation -> Exceptions.severity
