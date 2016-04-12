(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Environment for temporary identifiers used in instructions.
    Lazy implementation: only created when actually used. *)


type t

val create : Cfg.Procdesc.t -> t
val create_from_idenv : t -> Cfg.Procdesc.t -> t
val lookup : t -> Ident.t -> Sil.exp option
val expand_expr : t -> Sil.exp -> Sil.exp

val exp_is_temp : t -> Sil.exp -> bool

(** Stronger version of expand_expr which also expands a temporary variable. *)
val expand_expr_temps : t -> Cfg.Node.t -> Sil.exp -> Sil.exp
