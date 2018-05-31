(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Environment for temporary identifiers used in instructions.
    Lazy implementation: only created when actually used. *)

type t

val create : Procdesc.t -> t

val expand_expr : t -> Exp.t -> Exp.t

val exp_is_temp : t -> Exp.t -> bool

val expand_expr_temps : t -> Procdesc.Node.t -> Exp.t -> Exp.t
(** Stronger version of expand_expr which also expands a temporary variable. *)
