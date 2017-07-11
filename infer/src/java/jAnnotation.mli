(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open Javalib_pack

val translate_item : (JBasics.annotation * Javalib.visibility) list -> Annot.Item.t
(** Translate an item annotation. *)

val translate_method : Javalib.method_annotations -> Annot.Method.t
(** Translate a method annotation. *)
