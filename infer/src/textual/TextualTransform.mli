(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val remove_internal_calls : Textual.Module.t -> Textual.Module.t
(* generates enough intermediate Let instructions to make the procdesc free
   of sub-expressions containing regular calls.
   Example:
     n2 = m(n0, g3(n1))
   -->
     n3 = g3(n1)
     n2 = m(n0, n3)
*)

val let_propagation : Textual.Module.t -> Textual.Module.t

val out_of_ssa : Textual.Module.t -> Textual.Module.t
