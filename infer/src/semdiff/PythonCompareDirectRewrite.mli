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

module Condition : sig
  type predicate = Equals [@@deriving equal]

  type t = {predicate: predicate; args: Pattern.t list; value: bool} [@@deriving equal]
end

module Rules : sig
  type rule = {lhs: Pattern.t; rhs: Pattern.t; condition: Condition.t option}

  type t = {ignore: Pattern.t list; rewrite: rule list; accept: rule list} [@@deriving equal]

  val pp : F.formatter -> t -> unit
end

val missing_python_type_annotations_config : Rules.t

val ast_diff :
     debug:bool
  -> config:Rules.t
  -> ?filename1:string
  -> ?filename2:string
  -> string
  -> string
  -> Diff.explicit list
