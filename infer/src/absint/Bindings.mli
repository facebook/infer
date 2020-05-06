(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

include AbstractDomain.S

val empty : t

val add : Var.t -> HilExp.AccessExpression.t -> t -> t

val exit_scope : Var.t -> t -> t * Var.t list
(** returns the new bindings as well as a list of variables that became dead *)

val resolve : t -> Var.t -> HilExp.AccessExpression.t option

val fold : t -> init:'accum -> f:(Var.t -> HilExp.AccessExpression.t -> 'accum -> 'accum) -> 'accum
