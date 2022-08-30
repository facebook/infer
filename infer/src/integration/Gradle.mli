(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type javac_data = {files: string list; opts: string list}

val parse_gradle_line : kotlin:bool -> line:string -> javac_data
(** parse a single gradle output line and extract files and javac opts *)

val capture : prog:string -> args:string list -> unit
(** do a gradle capture with the given prog (i.e. gradle) and args *)
