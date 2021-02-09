(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Arithmetic terms *)

include module type of Arithmetic_intf

module Representation
    (Var : Var_intf.S)
    (Indeterminate : INDETERMINATE with type var := Var.t) :
  REPRESENTATION with type var := Var.t with type trm := Indeterminate.t
