(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module F = Format

type t = unit [@@deriving compare, equal]

let pp f () = F.pp_print_string f "()"

let has_config_read _ = false

let of_string ~config_type:_ _ = ()
