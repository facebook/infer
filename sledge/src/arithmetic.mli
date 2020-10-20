(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Arithmetic terms *)

include module type of Arithmetic_intf

module Representation (Indeterminate : INDETERMINATE) :
  REPRESENTATION
    with type var := Indeterminate.var
    with type trm := Indeterminate.trm
