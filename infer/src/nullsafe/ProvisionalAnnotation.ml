(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type t =
  | Field of {field_name: string}
  | Method of Procname.Java.t
  | Param of {method_info: Procname.Java.t; num: int}
[@@deriving compare]
