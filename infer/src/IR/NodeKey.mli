(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t

val to_string : t -> string

val compute :
  'node -> simple_key:('node -> 'simple_key) -> succs:'node list -> preds:'node list -> t

val of_frontend_node_key : string -> t
