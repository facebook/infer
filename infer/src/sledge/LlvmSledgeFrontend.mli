(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Translate LLVM to LLAIR *)

module F = Format

exception Invalid_llvm of string

val translate : ?dump_bitcode:string -> string -> Llair.program
(** Translate the compilation units in the named (llvm or bitcode) files to LLAIR. Attempts to raise
    [Invalid_llvm] when the input is invalid LLVM. *)

val read_and_reset_unimplemented_funcs_count : unit -> int
(** Number of functions whose translation raised [Unimplemented] and was replaced with
    [Func.mk_undefined] since the previous read. Lets [LlvmFrontend.log_stats] separate "we tried
    and gave up" stub-outs from "external declaration, expected" stub-outs. *)
