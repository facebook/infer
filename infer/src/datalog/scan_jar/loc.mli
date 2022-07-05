(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! Core
open Javalib_pack

val get_class_loc : JCode.jcode Javalib.jclass -> int
(** Given a Java class, returns the estimated lines of source code. *)
