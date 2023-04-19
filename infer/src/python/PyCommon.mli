(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val builtin_name : string -> Textual.qualified_procname
(** Helper function to encode known builtin names correctly *)

val python_int : Textual.qualified_procname
(** Encoding of Python [int] type. Since Python integers are of arbitrary precision, they are not
    modeled directly with [int]. *)

val python_string : Textual.qualified_procname
(** Encoding of Python [str] type. *)

val python_tuple : Textual.qualified_procname
(** Encoding of Python [tuple] type. It is the raw "untyped" one where every item is of type
    [object]. *)

val pyObject : Textual.Typ.t
(** [object] is the top type of Python. It helps us when no type information is available. *)

val pyInt : Textual.Typ.t
(** Textual encoding of the primitive Python type [int] *)

val pyString : Textual.Typ.t
(** Textual encoding of the primitive Python type [str] *)

val mk_int : int64 -> Textual.Exp.t
(** Helper function to define typed Textual expression for literal integers. *)

val mk_string : string -> Textual.Exp.t
(** Helper function to define typed Textual expression for literal strings. *)

val mk_is_true : Textual.Exp.t -> Textual.Exp.t

module Builtins : sig
  (** This module keeps track of the builtins used by a code unit. Only the necessary the Textual
      declarations are generated. Note that primitive wrappers are always generated ([python_int],
      ...) *)
  type t

  val to_textual : t -> Textual.Module.decl list
  (** Encode a set of builtin declarations into Textual declarations *)

  val register : t -> string -> t
  (** Call this function when a builtin is spotted in the code *)

  (* TODO: once toplevel definitions are supported , one can shadow builtins, so we'll need to
     take this into account. *)

  val is_builtin : string -> bool
  (** Check if a function name is a known buitlin. *)

  (* An empty set of builtins *)
  val empty : t
end
