(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val python_ast_diff :
     debug:bool
  -> config:SemdiffDirectEngine.Rules.t
  -> ?filename1:string
  -> ?filename2:string
  -> string
  -> string
  -> Diff.explicit list

val semdiff : config_files:string list -> previous_file:string -> current_file:string -> unit
