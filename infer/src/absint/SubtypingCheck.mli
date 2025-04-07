(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** {2 Subtype checking} *)

val check_subtype : Tenv.t -> Typ.t -> Typ.t -> bool
(** check_subtype t1 t2 checks whether t1 is a subtype of t2, given the type environment tenv. *)
