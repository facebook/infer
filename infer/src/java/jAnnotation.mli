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

open Javalib_pack


(** Translate an item annotation. *)
val translate_item : (JBasics.annotation * Javalib.visibility) list -> Sil.item_annotation

(** Translate a method annotation. *)
val translate_method : Procname.java -> Javalib.method_annotations -> Sil.method_annotation
