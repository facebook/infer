(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Abstraction for Arrays *)

val abstract_array_check : Tenv.t -> Prop.normal Prop.t -> Prop.normal Prop.t
(** Apply array abstraction and check the result *)

val array_abstraction_performed : bool ref
(** Remember whether array abstraction was performed (to be reset before calling Abs.abstract) *)

val remove_redundant_elements : Tenv.t -> Prop.normal Prop.t -> Prop.normal Prop.t
(** remove redundant elements in an array *)
