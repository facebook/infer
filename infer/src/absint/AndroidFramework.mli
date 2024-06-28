(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Android lifecycle types and their lifecycle methods that are called by the framework *)

(* val drawable_prefix : string *)
(** prefix for Drawable fields in generated resources *)

val is_autocloseable : Tenv.t -> Typ.Name.t -> bool

val is_view : Tenv.t -> Typ.Name.t -> bool
(** return true if [typename] <: android.view.View *)

val is_fragment : Tenv.t -> Typ.Name.t -> bool
