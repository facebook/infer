(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Environment for temporary identifiers used in instructions.
    Lazy implementation: only created when actually used. *)

type t

val create : Procdesc.t -> t

val lookup : t -> Ident.t -> Exp.t option

val expand_expr : t -> Exp.t -> Exp.t

val exp_is_temp : t -> Exp.t -> bool

val expand_expr_temps : t -> Procdesc.Node.t -> Exp.t -> Exp.t
(** Stronger version of expand_expr which also expands a temporary variable. *)
