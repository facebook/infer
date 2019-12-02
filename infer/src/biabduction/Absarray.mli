(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val array_clean_new_index : bool -> Exp.t -> Exp.t
(** This function should be used before adding a new index to Earray. The [exp] is the newly created
    index. This function "cleans" [exp] according to whether it is the footprint or current part of
    the prop. The function faults in the re - execution mode, as an internal check of the tool. *)

(** Abstraction for Arrays *)

val abstract_array_check : Tenv.t -> Prop.normal Prop.t -> Prop.normal Prop.t
(** Apply array abstraction and check the result *)

val array_abstraction_performed : bool ref
(** Remember whether array abstraction was performed (to be reset before calling Abs.abstract) *)

val remove_redundant_elements : Tenv.t -> Prop.normal Prop.t -> Prop.normal Prop.t
(** remove redundant elements in an array *)
