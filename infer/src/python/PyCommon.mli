(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val type_name : ?loc:Textual.Location.t -> string -> Textual.TypeName.t

val proc_name : ?loc:Textual.Location.t -> string -> Textual.ProcName.t

val var_name : ?loc:Textual.Location.t -> string -> Textual.VarName.t

val node_name : ?loc:Textual.Location.t -> string -> Textual.NodeName.t

val field_name : ?loc:Textual.Location.t -> string -> Textual.FieldName.t

val qualified_procname :
  enclosing_class:Textual.TypeName.t -> Textual.ProcName.t -> Textual.qualified_procname

val builtin_name : string -> Textual.qualified_procname
(** Helper function to encode known builtin names correctly *)

val python_int : Textual.qualified_procname
(** Encoding of Python [int] type. Since Python integers are of arbitrary precision, they are not
    modeled directly with [int]. *)

val python_float : Textual.qualified_procname
(** Encoding of Python [float] type. *)

val python_string : Textual.qualified_procname
(** Encoding of Python [str] type. *)

val python_bytes : Textual.qualified_procname
(** Encoding of Python [bytes] type. *)

val python_bool : Textual.qualified_procname
(** Encoding of Python [bool] type. *)

val python_tuple : Textual.qualified_procname
(** Encoding of Python [tuple] type. It is the raw "untyped" one where every item is of type
    [object]. *)

val pyObject : Textual.Typ.t
(** [object] is the top type of Python. It helps us when no type information is available. *)

val pyCode : Textual.Typ.t
(** [code] is a builtin Python type to describe any code (function, class, ...) object *)

val pyClass : Textual.Typ.t
(** Like [pyCode] but for class creation *)

val pyIterItem : Textual.Typ.t
(** Wrapper type to implement [next] access on an iterator. Declared as [pyIterItemStruct] *)

val py_iter_item_has_item : Textual.qualified_fieldname

val py_iter_item_next_item : Textual.qualified_fieldname

val pyIterItemStruct : Textual.Struct.t
(** Textual structure with two fields. [has_item] indicates if something was read from the iterator
    or if it was empty. If [has_item] is true, [next_item] holds the value read from the iterator. *)

val pyMethod : Textual.Typ.t
(** Wrapper type to implement [LOAD_METHOD]. Declared as [pyMethodStruct] *)

val py_method_code : Textual.qualified_fieldname [@@warning "-unused-value-declaration"]

val py_method_self : Textual.qualified_fieldname [@@warning "-unused-value-declaration"]

val pyMethodStruct : Textual.Struct.t
(** Textual structure with three fields.

    - [code] stores the code of the method called, or an arbitrary callable the method was not found
    - [self] stores a reference to the instance where the method was found, or [NULL] if the method
      was not found *)

val pyInt : Textual.Typ.t
(** Textual encoding of the primitive Python type [int] *)

val pyString : Textual.Typ.t
(** Textual encoding of the primitive Python type [str] *)

val pyBytes : Textual.Typ.t
(** Textual encoding of the primitive Python type [bytes] *)

val pyBool : Textual.Typ.t
(** Textual encoding of the primitive Python type [bool] *)

val pyFloat : Textual.Typ.t
(** Textual encoding of the primitive Python type [float] *)

val pyNone : Textual.Typ.t
(** Textual encoding of the primitive Python type [None] *)

val mk_int : int64 -> Textual.Exp.t
(** Helper function to define typed Textual expression for literal integers. *)

val mk_float : float -> Textual.Exp.t
(** Helper function to define typed Textual expression for literal floats. *)

val read_int : Textual.Exp.t -> Z.t option
(** Helper function to extract an integer from a Textual expression, if possible. *)

val mk_string : string -> Textual.Exp.t
(** Helper function to define typed Textual expression for literal strings. *)

val mk_bytes : bytes -> Textual.Exp.t
(** Helper function to define typed Textual expression for literal bytes. *)

val mk_bool : bool -> Textual.Exp.t
(** Helper function to define typed Textual expression for boolean. *)

val unknown_global : string -> string
(** Wrap a variable name into a special enclosing class when a global's origin can't be found. *)

(** Encoding of some type annotation like [x: int] *)
type annotated_name = {name: string; annotation: string}

(** Method declaration info (name, signature, ... *)
type method_info =
  { name: string
  ; raw_qualified_name: string
  ; code: FFI.Constant.t
  ; signature: annotated_name list
  ; flags: int }

val toplevel_function : string
