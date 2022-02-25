(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Intrinsic function names *)

type t =
  [ (* threads *)
    `sledge_thread_create
  | `sledge_thread_resume
  | `sledge_thread_join ]
[@@deriving compare, equal, sexp_of, enumerate]
