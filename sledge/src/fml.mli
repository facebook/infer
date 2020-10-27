(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Formulas *)

open Propositional_intf
include FORMULA with type trm := Trm.t
module Set : FORMULA_SET with type elt := t with type t = set

val tt : t
val ff : t
val bool : bool -> t
val _Eq0 : Trm.t -> t
val _Pos : Trm.t -> t
val _Eq : Trm.t -> Trm.t -> t
val vars : t -> Var.t iter
