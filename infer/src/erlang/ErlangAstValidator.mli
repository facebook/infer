(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Enforce additional invariants and constraints on the AST based on
    https://erlang.org/doc/apps/erts/absform.html *)

val validate : ErlangAst.module_ -> bool
(** Validate a module. *)
