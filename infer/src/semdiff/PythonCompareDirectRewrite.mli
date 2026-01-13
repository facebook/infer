(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module Var : sig
  type t = private string [@@deriving compare, equal]

  val pp : F.formatter -> t -> unit

  val of_string : string -> t
end

module Name : sig
  type t = private string [@@deriving compare, equal]

  val pp : F.formatter -> t -> unit

  val of_string : string -> t
end

module Pattern : sig
  type t =
    | Var of Var.t
    | AstNode of PythonSourceAst.Node.t
    | Node of {name: Name.t; args: (Name.t * t) list}
    | List of t list
end

val semdiff : string -> string -> unit

val test_ast_diff : debug:bool -> string -> string -> Diff.explicit list
