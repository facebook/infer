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

module Pp : sig
  val option : (F.formatter -> 'a -> unit) -> F.formatter -> 'a option -> unit
  [@@warning "-unused-value-declaration"]

  val comma_seq : (F.formatter -> 'a -> unit) -> F.formatter -> 'a list -> unit
  [@@warning "-unused-value-declaration"]

  val semicolon_seq : (F.formatter -> 'a -> unit) -> F.formatter -> 'a list -> unit
  [@@warning "-unused-value-declaration"]

  val array : (F.formatter -> 'a -> unit) -> F.formatter -> 'a array -> unit
  [@@warning "-unused-value-declaration"]

  val iarray : (F.formatter -> 'a -> unit) -> F.formatter -> 'a IArray.t -> unit
  [@@warning "-unused-value-declaration"]
end
