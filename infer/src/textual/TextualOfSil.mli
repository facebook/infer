(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val from_c : SourceFile.t -> string -> Tenv.t -> Cfg.t -> unit
(** generate a .sil (named [filename]) file from the source file, containing all the functions and
    type declarations in the given cfg *)

val from_java : filename:string -> Tenv.t -> Cfg.t -> unit
(** generate a .sil file with name [filename] containing all the functions in the given cfg (Java)
*)

val to_string :
     lang:Textual.Lang.t
  -> ?sil_source_file:SourceFile.t
  -> filename:string
  -> Tenv.t
  -> Cfg.t
  -> string
(** return textual SIL as a string *)
