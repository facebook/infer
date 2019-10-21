(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(** Dereference rule should be checked every type an object is dereferenced.
    The rule checks if the reference is nullable.
 *)

type violation [@@deriving compare]

val check : is_strict_mode:bool -> Nullability.t -> (unit, violation) result

type dereference_type =
  | MethodCall of Typ.Procname.t
  | AccessToField of Typ.Fieldname.t
  | AccessByIndex of {index_desc: string}
  | ArrayLengthAccess
[@@deriving compare]

val violation_description :
     violation
  -> dereference_type
  -> nullable_object_descr:string option
  -> origin_descr:string
  -> string
