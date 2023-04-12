(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val builtin_name : string -> Textual.qualified_procname

val python_int : Textual.qualified_procname

val python_string : Textual.qualified_procname

val python_tuple : Textual.qualified_procname

(* Pointer to pyObject *)
val pyObject : Textual.Typ.t

val pyInt : Textual.Typ.t

val pyString : Textual.Typ.t

val mk_int : int64 -> Textual.Exp.t

val mk_string : string -> Textual.Exp.t

module Builtins : sig
  (* With this type we keep track of the builtins some piece of code is using. This enables us to
     only generate the textual declaration we need.
     Note that we always generate the primite wrapper (python_int, ...) *)
  type t

  val to_textual : t -> Textual.Module.decl list

  (* Call this function when a builtin is spotted in the code *)
  val register : t -> string -> t

  (* Is a function name a builtin.
     TODO: once we get toplevel definitions, one can shadow builtins, so we'll need to take this
     into account. *)
  val is_builtin : string -> bool

  (* An empty set of builtins *)
  val empty : t
end
