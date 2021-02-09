(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Renaming Substitutions: injective maps from variables to variables *)

include module type of Subst_intf

module Make (Var : VAR) :
  S with type var := Var.t with type set := Var.Set.t
