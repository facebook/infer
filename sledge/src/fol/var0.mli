(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Var_intf

(** Variables, parameterized over their representation *)
module Make (R : REPR) : VAR with type t = R.t
