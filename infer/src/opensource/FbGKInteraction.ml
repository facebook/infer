(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module F = Format

module ConfigName = struct
  type t = unit

  let compare _ _ = 0

  let equal _ _ = true

  let pp f () = F.pp_print_string f "()"
end

let get_config _ = None

let is_config_check _ _ = false

let is_marker_start _ _ = false

let is_marker_end _ _ = false
