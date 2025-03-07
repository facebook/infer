(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Global variables *)

open! NS

type t = private {name: Global.t; init: LlairExp.t option; loc: Loc.t}
[@@deriving compare, equal, sexp]

val pp : t pp

include Invariant.S with type t := t

val mk : ?init:LlairExp.t -> Global.t -> Loc.t -> t
