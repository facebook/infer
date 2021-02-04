(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Module for Procedure Names. *)

(** Type of csharp procedure names. *)
module CSharp : sig
  type kind =
    | Non_Static
        (** in Java, procedures called with invokevirtual, invokespecial, and invokeinterface *)
    | Static  (** in Java, procedures called with invokestatic *)

  type t [@@deriving compare]

  val constructor_method_name : string

  val get_method : t -> string
  (** Return the method name of a csharp procedure name. *)

  val get_class_type_name : t -> Typ.Name.t
  (** Return the class name as a typename of a java procedure name. *)

  val get_return_typ : t -> Typ.t
  (** Return the return type of [pname_csharp]. return Tvoid if there's no return type *)

  val get_class_name : t -> string
  (** Return the class name of a java procedure name. *)

  val is_generated : t -> bool
  (** Check if the proc name comes from generated code *)
end

(** Type of java procedure names. *)
module Java : sig
  type kind =
    | Non_Static
        (** in Java, procedures called with invokevirtual, invokespecial, and invokeinterface *)
    | Static  (** in Java, procedures called with invokestatic *)

  type t [@@deriving compare]

  val to_simplified_string : ?withclass:bool -> t -> string

  val constructor_method_name : string

  val class_initializer_method_name : string

  val replace_method_name : string -> t -> t
  (** Replace the method name of an existing java procname. *)

  val replace_parameters : Typ.t list -> t -> t
  (** Replace the parameters of a java procname. *)

  val replace_return_type : Typ.t -> t -> t
  (** Replace the method of a java procname. *)

  val get_class_name : t -> string
  (** Return the fully qualified class name of a java procedure name (package + class name) *)

  val get_class_type_name : t -> Typ.Name.t
  (** Return the class name as a typename of a java procedure name. *)

  val get_simple_class_name : t -> string
  (** Return the simple class name of a java procedure name (i.e. name without the package info). *)

  val get_package : t -> string option
  (** Return the package name of a java procedure name. *)

  val get_method : t -> string
  (** Return the method name of a java procedure name. *)

  val get_parameters : t -> Typ.t list
  (** Return the parameters of a java procedure name. *)

  val get_return_typ : t -> Typ.t
  (** Return the return type of [pname_java]. return Tvoid if there's no return type *)

  val is_constructor : t -> bool
  (** Whether the method is constructor *)

  val is_access_method : t -> bool
  (** Check if the procedure name is an acess method (e.g. access$100 used to access private members
      from a nested class. *)

  val is_autogen_method : t -> bool
  (** Check if the procedure name is of an auto-generated/synthetic method. *)

  val is_anonymous_inner_class_constructor_exn : t -> bool
  (** Check if the procedure name is an anonymous inner class constructor. Throws if it is not a
      Java type *)

  val is_close : t -> bool
  (** Check if the method name is "close". *)

  val is_static : t -> bool
  (** Check if the java procedure is static. *)

  val is_vararg : t -> bool
  (** Check if the proc name has the type of a java vararg. Note: currently only checks that the
      last argument has type Object[]. *)

  val is_lambda : t -> bool
  (** Check if the proc name comes from a lambda expression *)

  val is_generated : t -> bool
  (** Check if the proc name comes from generated code *)

  val is_class_initializer : t -> bool
  (** Check if this is a class initializer. *)

  val get_class_initializer : Typ.Name.t -> t
  (** Given a java class, generate the procname of its static initializer. *)

  val is_external : t -> bool
  (** Check if the method belongs to one of the specified external packages *)
end

module Parameter : sig
  (** Type for parameters in clang procnames, [Some name] means the parameter is of type pointer to
      struct, with [name] being the name of the struct, [None] means the parameter is of some other
      type. *)
  type clang_parameter = Typ.Name.t option [@@deriving compare, equal]

  (** Type for parameters in procnames, for java and clang. *)
  type t = JavaParameter of Typ.t | ClangParameter of clang_parameter | CSharpParameter of Typ.t
  [@@deriving compare, equal]

  val of_typ : Typ.t -> clang_parameter
end

module ObjC_Cpp : sig
  type kind =
    | CPPMethod of {mangled: string option}
    | CPPConstructor of {mangled: string option; is_constexpr: bool}
    | CPPDestructor of {mangled: string option}
    | ObjCClassMethod
    | ObjCInstanceMethod
    | ObjCInternalMethod
  [@@deriving compare]

  (** Type of Objective C and C++ procedure names: method signatures. *)
  type t =
    { class_name: Typ.Name.t
    ; kind: kind
    ; method_name: string
    ; parameters: Parameter.clang_parameter list
    ; template_args: Typ.template_spec_info }
  [@@deriving compare]

  val make :
    Typ.Name.t -> string -> kind -> Typ.template_spec_info -> Parameter.clang_parameter list -> t
  (** Create an objc procedure name from a class_name and method_name. *)

  val get_class_name : t -> string

  val get_class_type_name : t -> Typ.Name.t [@@warning "-32"]

  val get_class_qualifiers : t -> QualifiedCppName.t

  val objc_method_kind_of_bool : bool -> kind
  (** Create ObjC method type from a bool is_instance. *)

  val is_objc_constructor : string -> bool
  (** Check if this is a constructor method in Objective-C. *)

  val is_objc_dealloc : string -> bool
  (** Check if this is a dealloc method in Objective-C. *)

  val is_destructor : t -> bool
  (** Check if this is a dealloc method. *)

  val is_inner_destructor : t -> bool
  (** Check if this is a frontend-generated "inner" destructor (see D5834555/D7189239) *)

  val is_constexpr : t -> bool
  (** Check if this is a constexpr function. *)

  val is_cpp_lambda : t -> bool
  (** Return whether the procname is a cpp lambda. *)
end

module C : sig
  (** Type of c procedure names. *)
  type t = private
    { name: QualifiedCppName.t
    ; mangled: string option
    ; parameters: Parameter.clang_parameter list
    ; template_args: Typ.template_spec_info }

  val c :
    QualifiedCppName.t -> string -> Parameter.clang_parameter list -> Typ.template_spec_info -> t
  (** Create a C procedure name from plain and mangled name. *)

  val is_make_shared : t -> bool
end

module Block : sig
  (** Type of Objective C block names. *)
  type block_name = string

  type t = {name: block_name; parameters: Parameter.clang_parameter list} [@@deriving compare]

  val make : block_name -> Parameter.clang_parameter list -> t
end

(** Type of procedure names. WithBlockParameters is used for creating an instantiation of a method
    that contains block parameters and it's called with concrete blocks. For example:
    [foo(Block block) {block();}] [bar() {foo(my_block)}] is executed as
    [foo_my_block() {my_block(); }] where foo_my_block is created with WithBlockParameters (foo,
    [my_block]) *)
type t =
  | CSharp of CSharp.t
  | Java of Java.t
  | C of C.t
  | Linters_dummy_method
  | Block of Block.t
  | ObjC_Cpp of ObjC_Cpp.t
  | WithBlockParameters of t * Block.t list
[@@deriving compare, yojson_of]

val block_of_procname : t -> Block.t

val equal : t -> t -> bool

val get_class_type_name : t -> Typ.Name.t option

val get_class_name : t -> string option

val get_parameters : t -> Parameter.t list

val replace_parameters : Parameter.t list -> t -> t

val parameter_of_name : t -> Typ.Name.t -> Parameter.t

val is_java_access_method : t -> bool

val is_java_class_initializer : t -> bool

val is_java_anonymous_inner_class_method : t -> bool

val is_java_autogen_method : t -> bool

val is_objc_method : t -> bool

(** Hash tables with proc names as keys. *)
module Hash : Caml.Hashtbl.S with type key = t

module LRUHash : LRUHashtbl.S with type key = t

module HashQueue : Hash_queue.S with type key = t

(** Maps from proc names. *)
module Map : PrettyPrintable.PPMap with type key = t

(** Sets of proc names. *)
module Set : PrettyPrintable.PPSet with type elt = t

module SQLite : sig
  val serialize : t -> Sqlite3.Data.t

  val deserialize : Sqlite3.Data.t -> t

  val clear_cache : unit -> unit
end

module SQLiteList : SqliteUtils.Data with type t = t list

(** One-sized cache for one procedure at a time. Returns getter and setter. *)
module UnitCache : sig
  val create : unit -> (t -> 'a option) * (t -> 'a -> unit)
end

val make_java :
     class_name:Typ.Name.t
  -> return_type:Typ.t option
  -> method_name:string
  -> parameters:Typ.t list
  -> kind:Java.kind
  -> unit
  -> t
(** Create a Java procedure name. *)

val make_csharp :
     class_name:Typ.Name.t
  -> return_type:Typ.t option
  -> method_name:string
  -> parameters:Typ.t list
  -> kind:CSharp.kind
  -> unit
  -> t
(** Create a CSharp procedure name. *)

val make_objc_dealloc : Typ.Name.t -> t
(** Create a Objective-C dealloc name. This is a destructor for an Objective-C class. This procname
    is given by the class name, since it is always an instance method with the name "dealloc" *)

val make_objc_copyWithZone : is_mutable:bool -> Typ.Name.t -> t
(** Create an Objective-C method for copyWithZone: or mutableCopyWithZone: according to is_mutable. *)

val empty_block : t
(** Empty block name. *)

val get_language : t -> Language.t
(** Return the language of the procedure. *)

val get_method : t -> string
(** Return the method/function of a procname. *)

val is_objc_block : t -> bool
(** Return whether the procname is a block procname. *)

val is_objc_dealloc : t -> bool
(** Return whether the dealloc method of an Objective-C class. *)

val is_c_method : t -> bool
(** Return true this is an Objective-C/C++ method name. *)

val is_constructor : t -> bool
(** Check if this is a constructor. *)

val is_csharp : t -> bool
(** Check if this is a CSharp procedure name. *)

val is_java : t -> bool
(** Check if this is a Java procedure name. *)

val as_java_exn : explanation:string -> t -> Java.t
(** Converts to a Java.t. Throws if [is_java] is false *)

val with_block_parameters : t -> Block.t list -> t
(** Create a procedure name instantiated with block parameters from a base procedure name and a list
    of block procedures. *)

val objc_cpp_replace_method_name : t -> string -> t

val is_infer_undefined : t -> bool
(** Check if this is a special Infer undefined procedure. *)

val get_global_name_of_initializer : t -> string option
(** Return the name of the global for which this procedure is the initializer if this is an
    initializer, None otherwise. *)

val pp : Format.formatter -> t -> unit
(** Pretty print a proc name for the user to see. *)

val to_string : t -> string
(** Convert a proc name into a string for the user to see. *)

val describe : Format.formatter -> t -> unit
(** to use in user messages *)

val replace_class : t -> Typ.Name.t -> t
(** Replace the class name component of a procedure name. In case of Java, replace package and class
    name. *)

val is_method_in_objc_protocol : t -> bool

val pp_simplified_string : ?withclass:bool -> F.formatter -> t -> unit
(** Pretty print a proc name as an easy string for the user to see in an IDE. *)

val to_simplified_string : ?withclass:bool -> t -> string
(** Convert a proc name into an easy string for the user to see in an IDE. *)

val from_string_c_fun : string -> t
(** Convert a string to a c function name. *)

val hashable_name : t -> string
(** Convert the procedure name in a format suitable for computing the bug hash. *)

val pp_unique_id : F.formatter -> t -> unit
(** Print a proc name as a unique identifier. *)

val to_unique_id : t -> string
(** Convert a proc name into a unique identifier. *)

val to_filename : t -> string
(** Convert a proc name to a filename. *)

val get_qualifiers : t -> QualifiedCppName.t
(** get qualifiers of C/objc/C++ method/function *)

module Normalizer : HashNormalizer.S with type t = t
