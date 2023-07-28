(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type textual =
  | IsTrue
  | BinaryAdd
  | PythonCall
  | PythonClass
  | PythonCode
  | PythonIter
  | PythonIterNext
[@@deriving compare]

type builtin

val textual : textual -> builtin

val of_string : string -> builtin option

val to_proc_name : builtin -> Textual.qualified_procname

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
