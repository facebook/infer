(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(** Provisional annotation is an imaginary [@Nullable] annotation that is added to a class element.
    * (It implies the corresponding element is NOT annotated as [@Nullable] in the source code.) *
    The purpose of provisional annotation is to compute Nullability graph: what would happen if such
    and such * element was annotated as [@Nullable]. *)

type t =
  | Field of {field_name: Fieldname.t}
  | Method of Procname.Java.t
  | Param of {method_info: Procname.Java.t; num: int}
[@@deriving compare]

val pp : Format.formatter -> t -> unit
