(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Capture module for the json analysis in the capture phase *)

open! IStd

val capture : cfg_json:string -> tenv_json:string -> unit
(** Run the capture of the files for which we have cfg in [cfg_json], type environment [tenv_json]
    in and [changed_files], if specified. *)
