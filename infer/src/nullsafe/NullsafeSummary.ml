(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Summary of per-procedure nullsafe result analysis *)

type t = {issues: TypeErr.err_instance list}

let pp fmt {issues} = Format.fprintf fmt "Issues: %d" (List.length issues)
