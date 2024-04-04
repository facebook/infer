(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module SMap : Caml.Map.S with type key = string

module SSet : Caml.Set.S with type elt = string

module IMap : Caml.Map.S with type key = int

val proc_name : ?loc:Textual.Location.t -> string -> Textual.ProcName.t

val var_name : ?loc:Textual.Location.t -> string -> Textual.VarName.t

val node_name : ?loc:Textual.Location.t -> string -> Textual.NodeName.t

val field_name : ?loc:Textual.Location.t -> string -> Textual.FieldName.t

val mk_type : string -> Textual.Typ.t

val qualified_procname :
  enclosing_class:Textual.TypeName.t -> Textual.ProcName.t -> Textual.QualifiedProcName.t

val builtin_name : string -> Textual.QualifiedProcName.t
(** Helper function to encode known builtin names correctly *)

val python_int : Textual.QualifiedProcName.t
(** Encoding of Python [int] type. Since Python integers are of arbitrary precision, they are not
    modeled directly with [int]. *)

val python_float : Textual.QualifiedProcName.t
(** Encoding of Python [float] type. *)

val python_string : Textual.QualifiedProcName.t
(** Encoding of Python [str] type. *)

val python_bytes : Textual.QualifiedProcName.t
(** Encoding of Python [bytes] type. *)

val python_bool : Textual.QualifiedProcName.t
(** Encoding of Python [bool] type. *)

val python_tuple : Textual.QualifiedProcName.t
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

val pyList : Textual.Typ.t
(** Python's builtin [list] type *)

val pyMap : Textual.Typ.t
(** Python's builtin [map] type *)

val pySet : Textual.Typ.t
(** Python's builtin [set] type *)

val pyTuple : Textual.Typ.t
(** Python's builtin [tuple] type *)

val mk_int : Z.t -> Textual.Exp.t
(** Helper function to define typed Textual expression for literal integers. *)

val mk_float : float -> Textual.Exp.t
(** Helper function to define typed Textual expression for literal floats. *)

val mk_string : string -> Textual.Exp.t
(** Helper function to define typed Textual expression for literal strings. *)

val get_string : Textual.Exp.t -> string option
(** Helper to get back a string built with [mk_string] *)

val get_tuple_as_list : Textual.Exp.t -> Textual.Exp.t list option
(** Helper to get back the inner elements of a [python_tuple] *)

val mk_bytes : bytes -> Textual.Exp.t
(** Helper function to define typed Textual expression for literal bytes. *)

val mk_bool : bool -> Textual.Exp.t
(** Helper function to define typed Textual expression for boolean. *)

module Ident : sig
  (** Python uses qualified identifiers such as [sys.exit]. Locally defined names don't have any
      prefix, but we still add some in textual to deal with ambiguity. The only identifiers without
      any prefix are local variables. *)
  type t [@@deriving compare]

  val root : t -> t
  (** Strip the whole path of an identifier, only keeping its root *)

  val from_string : ?on:char -> ?global:bool -> ?loc:Textual.Location.t -> string -> t option

  val pp : Format.formatter -> t -> unit

  val fold :
       f_root:('a -> global:bool -> loc:Textual.Location.t -> string -> 'b)
    -> f_path:(string -> 'b -> 'b)
    -> init:'a
    -> t
    -> 'b

  val to_string : sep:string -> t -> string

  val to_qualified_procname : t -> Textual.QualifiedProcName.t

  val to_type_name : ?static:bool -> t -> Textual.TypeName.t

  val to_proc_name : t -> Textual.ProcName.t [@@warning "-unused-value-declaration"]

  val to_constructor : t -> Textual.ProcName.t

  val to_typ : t -> Textual.Typ.t

  val is_primitive_type : t -> bool

  val to_var_name : t -> Textual.VarName.t

  val mk_unknown_ident : ?loc:Textual.Location.t -> string -> t
  (** Wrap a variable name into a special enclosing class when a global's origin can't be found. *)

  val extend_unknown_ident : t -> t
  (** Wrap an existing identifier into a special enclosing class when a global's origin can't be
      found. *)

  val mk : ?global:bool -> ?loc:Textual.Location.t -> string -> t

  val extend : prefix:t -> string -> t

  val pop : t -> string * t option

  val mk_builtin : string -> t

  val is_imported_ABC : t -> bool
  (** Checks if an id is the standard [abc.ABC] metaclass name *)

  module Map : Caml.Map.S with type key = t

  module Set : Caml.Set.S with type elt = t
end

(** Encoding of some type annotation like [x: int] *)
type annotated_name = {name: string; annotation: Ident.t}

val pp_annotated_name : Format.formatter -> annotated_name -> unit
[@@warning "-unused-value-declaration"]

type signature = annotated_name list

val pp_signature : Format.formatter -> signature -> unit

val toplevel_function : string

val static_method : string

val static_companion : string -> string

module ABC : sig
  val abstract_method : string
end

val init__ : string
(** Name of the Python initialization method *)

val new__ : string
(** Name of the Python instantiation method *)

val exit : string
(** Name of the Python context exit method *)

val enter : string
(** Name of the Python context enter method *)

val return : string
(** Name of the binding name of return types of functions / methods *)

val class__ : string
(** Special name for [__class__] *)

val classcell : string
(** Special name for [__classcell__] *)

val annotations : string
(** Special name for [__annotations__] *)

val entry : string
(** Textual label name for entry points of function we synthesized (constructors, __init__, ...) *)

val self : string
[@@warning "-unused-value-declaration"]
(** Common name of the [self] argument in Python *)

(** Flags used by MAKE_FUNCTION *)
module MakeFunctionFlags : sig
  type flag = DefaultValues | DictDefaultValues | Annotations | Closure

  type t

  val pp : Format.formatter -> t -> unit

  val mk : int -> t

  val mem : t -> flag -> bool

  val set : t -> flag -> t [@@warning "-unused-value-declaration"]

  val unset : t -> flag -> t
end
