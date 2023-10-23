(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module Compare : sig
  type t = Lt | Le | Eq | Neq | Gt | Ge | In | NotIn | Is | IsNot | Exception | BAD
  [@@deriving compare, enumerate]

  val to_string : t -> string

  val pp : Format.formatter -> t -> unit
end

type binary_op =
  | Add
  | And
  | FloorDivide
  | LShift
  | MatrixMultiply
  | Modulo
  | Multiply
  | Or
  | Power
  | RShift
  | Subtract
  | TrueDivide
  | Xor
[@@deriving compare]

type unary_op = Positive | Negative | Not | Invert [@@deriving compare]

type builder = List | Set | Tuple | Map | String | Slice [@@deriving compare]

type textual =
  | IsTrue
  | Binary of binary_op
  | Inplace of binary_op
  | Unary of unary_op
  | PythonCall
  | PythonCallKW
  | PythonKWArg
  | PythonClass
  | PythonClassName
  | PythonCode
  | PythonIter
  | PythonIterNext
  | PythonBuild of builder
  | PythonIndex
  | PythonSubscriptGet
  | PythonSubscriptSet
  | PythonFormatRepr
  | PythonFormatStr
  | PythonFormatAscii
  | PythonFormat
  | CompareOp of Compare.t
[@@deriving compare]

type builtin

val textual : textual -> builtin

val of_string : string -> builtin option

val to_proc_name : builtin -> Textual.QualifiedProcName.t

module Set : sig
  (** This module keeps track of the builtins used by a code unit. Only the necessary Textual
      declarations are generated. Note that primitive wrappers are always generated ([python_int],
      ...) *)
  type t

  val supported_builtins : unit -> string list

  val empty : t

  val to_textual : t -> Textual.Module.decl list
  (** Encode a set of builtin declarations into Textual declarations *)

  val register : t -> builtin -> t

  val get_type : builtin -> Textual.Typ.t
end
