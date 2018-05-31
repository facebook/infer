(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Translate LLVM to LLAIR *)

exception Invalid_llvm of string

val translate : string -> Llair.t
(** Translate the compilation unit in the named (llvm or bitcode) file to
    LLAIR. Attempts to raise [Invalid_llvm] when the input is invalid LLVM. *)
