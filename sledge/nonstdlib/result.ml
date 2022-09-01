(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0
include Stdlib.Result

let pp fmt_ok pp_ok fmt_error pp_error fs = function
  | Ok x -> Format.fprintf fs fmt_ok pp_ok x
  | Error x -> Format.fprintf fs fmt_error pp_error x
