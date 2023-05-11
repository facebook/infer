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

val python_bool : Textual.qualified_procname
(** Encoding of Python [bool] type. *)

val python_tuple : Textual.qualified_procname
(** Encoding of Python [tuple] type. It is the raw "untyped" one where every item is of type
    [object]. *)

val pyObject : Textual.Typ.t
(** [object] is the top type of Python. It helps us when no type information is available. *)

val pyCode : Textual.Typ.t
(** [code] is a builtin Python type to describe any code (function, class, ...) object *)

val pyInt : Textual.Typ.t
(** Textual encoding of the primitive Python type [int] *)

val pyString : Textual.Typ.t
(** Textual encoding of the primitive Python type [str] *)

val pyBool : Textual.Typ.t
(** Textual encoding of the primitive Python type [bool] *)

val pyFloat : Textual.Typ.t
(** Textual encoding of the primitive Python type [float] *)

val pyNone : Textual.Typ.t
(** Textual encoding of the primitive Python type [None] *)

val mk_int : int64 -> Textual.Exp.t
(** Helper function to define typed Textual expression for literal integers. *)

val mk_string : string -> Textual.Exp.t
(** Helper function to define typed Textual expression for literal strings. *)

val mk_bool : bool -> Textual.Exp.t
(** Helper function to define typed Textual expression for boolean. *)

val global : string -> string
(** Wrap a variable name into the [global] namespace *)
