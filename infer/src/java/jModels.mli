(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Javalib_pack

val load_models : jar_filename:string -> unit
(** Sets the procnames in the given jar file as models *)

val get_classmap : unit -> JCode.jcode Javalib.interface_or_class JBasics.ClassMap.t
(** get map of model classes *)
