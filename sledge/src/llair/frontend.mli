(* Copyright (c) 2018 - present Facebook, Inc. All rights reserved.

   This source code is licensed under the BSD style license found in the
   LICENSE file in the root directory of this source tree. An additional
   grant of patent rights can be found in the PATENTS file in the same
   directory. *)

(** Translate LLVM to LLAIR *)

exception Invalid_llvm of string

val translate : string -> Llair.t
(** Translate the compilation unit in the named (llvm or bitcode) file to
    LLAIR. Attempts to raise [Invalid_llvm] when the input is invalid LLVM. *)
