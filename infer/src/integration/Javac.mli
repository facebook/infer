(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type compiler = Java | Javac [@@deriving compare]

val call_infer_javac_capture : javac_args:string list -> unit
(** perform a javac catpure given args to javac, this will shell out to 'infer capture -- javac *)

val capture : compiler -> prog:string -> args:string list -> unit
(** perform capture when given prog and args, this is the entrypoint for infer being called with
    'capture -- javac' *)
