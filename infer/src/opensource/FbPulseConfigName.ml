(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module F = Format

type t = unit

let compare _ _ = 0

let equal _ _ = true

let pp f () = F.pp_print_string f "()"

let has_config_read _ = false
