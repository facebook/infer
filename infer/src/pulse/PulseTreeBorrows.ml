(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type state = unit [@@deriving compare, equal]

let start () = ()

let pp fmt () = F.pp_print_string fmt "()"
