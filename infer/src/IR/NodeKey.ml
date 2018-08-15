(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = Caml.Digest.t

let to_string = Caml.Digest.to_hex

let compute node ~simple_key ~succs ~preds =
  let v = (simple_key node, List.rev_map ~f:simple_key succs, List.rev_map ~f:simple_key preds) in
  Utils.better_hash v


let of_frontend_node_key = Utils.better_hash
