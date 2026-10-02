(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val exe : prog:string -> args:string list -> unit
(** Given a clang command, normalize it via [clang -###] if needed to get a clear view of what work
    is being done and which source files are being compiled, if any. Pass the resulting files to
    compile to {!module-Capture} to be captured, i.e., parsed and translated into Infer's IR
    {!module-IR.Sil}. *)

val included_files : prog:string -> args:string list -> (string list, string) Result.t
(** The files included, directly or not, by the source file of the compilation command [prog args],
    according to the preprocessor of the clang used for capture, run with the arguments that the
    capture would use. The paths are absolute or relative to the current directory. *)
