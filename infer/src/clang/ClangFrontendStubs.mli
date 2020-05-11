(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** ****DO NOT USE DIRECTLY****

    This module is automatically [open]'d by the build system when compiling infer without clang
    support. The stubs implemented here do nothing. *)

module ClangQuotes : sig
  type style = EscapedDoubleQuotes | SingleQuotes | EscapedNoQuotes

  val mk_arg_file : string -> style -> string list -> string
end

module ClangTests : sig
  val tests : OUnit2.test list
end

module ClangWrapper : sig
  val exe : prog:string -> args:string list -> unit
end

module CTLParserHelper : sig
  val validate_al_files : unit -> (unit, string) Result.t
end

module RegisterCallback : sig
  val register_frontend_checks : unit -> unit
end
