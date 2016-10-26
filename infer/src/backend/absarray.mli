(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Abstraction for Arrays *)

(** Apply array abstraction and check the result *)
val abstract_array_check : Tenv.t -> Prop.normal Prop.t -> Prop.normal Prop.t

(** Remember whether array abstraction was performed (to be reset before calling Abs.abstract) *)
val array_abstraction_performed : bool ref

(** remove redundant elements in an array *)
val remove_redundant_elements : Tenv.t -> Prop.normal Prop.t -> Prop.normal Prop.t
