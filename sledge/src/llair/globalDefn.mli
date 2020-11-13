(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Global variables *)

type t = private {name: Global.t; init: Exp.t option; loc: Loc.t}
[@@deriving compare, equal, hash, sexp]

val pp : t pp

include Invariant.S with type t := t

val mk : ?init:Exp.t -> Global.t -> Loc.t -> t
