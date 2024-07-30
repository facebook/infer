(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Module for Procedure Names. *)

(** Level of verbosity of some to_string functions. *)
type detail_level = FullNameOnly | NameOnly | Non_verbose | Simple | Verbose

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

  type t [@@deriving compare, equal]

  val constructor_method_name : string

  val class_initializer_method_name : string

  val get_class_name : t -> string
  (** Return the fully qualified class name of a java procedure name (package + class name) *)

  val get_class_type_name : t -> Typ.Name.t
  (** Return the class name as a typename of a java procedure name. *)

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

  val is_close : t -> bool
  (** Check if the method name is "close". *)

  val is_static : t -> bool
  (** Check if the java procedure is static. *)

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
  type t =
    | JavaParameter of Typ.t
    | ClangParameter of clang_parameter
    | CSharpParameter of Typ.t
    | ErlangParameter
  [@@deriving compare, equal]

  val of_typ : Typ.t -> clang_parameter
end

module ObjC_Cpp : sig
  type mangled = string option [@@deriving compare]

  type kind =
    | CPPMethod of mangled
    | CPPConstructor of mangled
    | CPPDestructor of mangled
    | ObjCClassMethod
    | ObjCInstanceMethod
  [@@deriving compare]

  (** Type of Objective C and C++ procedure names: method signatures. *)
  type t =
    { class_name: Typ.Name.t
    ; kind: kind
    ; method_name: string
    ; parameters: Parameter.clang_parameter list
          (** NOTE: [parameters] should NOT include additional [this/self] or [__return_param]. *)
    ; template_args: Typ.template_spec_info }
  [@@deriving compare]

  val make :
    Typ.Name.t -> string -> kind -> Typ.template_spec_info -> Parameter.clang_parameter list -> t
  (** Create an objc procedure name from a class_name and method_name. *)

  val get_class_type_name : t -> Typ.Name.t [@@warning "-unused-value-declaration"]

  val get_class_qualifiers : t -> QualifiedCppName.t

  val objc_method_kind_of_bool : bool -> kind
  (** Create ObjC method type from a bool is_instance. *)

  val is_objc_constructor : string -> bool
  (** Check if this is a constructor method in Objective-C. *)

  val is_destructor : t -> bool
  (** Check if this is a dealloc method. *)

  val is_inner_destructor : t -> bool
  (** Check if this is a frontend-generated "inner" destructor (see D5834555/D7189239) *)
end

module C : sig
  (** Type of c procedure names. *)
  type t = Typ.c_function_sig

  val c : QualifiedCppName.t -> ?mangled:string -> Typ.template_spec_info -> t
  (** Create a C procedure name from plain and mangled name. *)

  val is_make_shared : t -> bool
end

module Block : sig
  (** Type of Objective C block names. *)

  type t = Typ.objc_block_sig [@@deriving compare, equal, yojson_of, sexp, hash, normalize]
end

module Erlang : sig
  type t = private {module_name: string; function_name: string; arity: int}
end

module Hack : sig
  (** Hack procedure is identified by the class and function names and its arity. The arity can be
      absent for external function declarations of the form [declare F.f(...): ...]

      TODO(arr): consider making the arity non-optional if we remove function declarations from the
      Tenv *)
  type t = private {class_name: HackClassName.t option; function_name: string; arity: int option}

  val get_class_name_as_a_string : t -> string option

  val is_xinit : t -> bool
end

module Python : sig
  (* TODO: revamp this once modules are implemented *)
  type t = private {class_name: PythonClassName.t option; function_name: string; arity: int option}

  type kind =
    | Fun of PythonClassName.t  (** Toplevel function name, or class constructor *)
    | Init of PythonClassName.t  (** Initialized of a class, like [C.__init__] *)
    | Other  (** Other methods *)
end

(** Type of procedure names. *)
type t =
  | Block of Block.t
  | C of C.t
  | CSharp of CSharp.t
  | Erlang of Erlang.t
  | Hack of Hack.t
  | Java of Java.t
  | ObjC_Cpp of ObjC_Cpp.t
  | Python of Python.t
[@@deriving compare, yojson_of, sexp, hash, normalize]

val compare_name : t -> t -> int
(** Similar to compare, but compares only names, except parameter types and template arguments. *)

val get_class_type_name : t -> Typ.Name.t option

val get_class_name : t -> string option

val python_classify : t -> Python.kind option
(** Classify a Python name into a [Python.kind] *)

val mk_python_init : t -> t
(** Turns a Python **toplevel** name into a valid initializer. E.g. it is used to turn a statement
    like [x = C(42)] into [C.__init__(x, 42)] *)

val get_parameters : t -> Parameter.t list

val replace_parameters : Parameter.t list -> t -> t

val parameter_of_name : t -> Typ.Name.t -> Parameter.t

val is_cpp_assignment_operator : t -> bool

val is_destructor : t -> bool

val is_java_static_method : t -> bool

val is_java_instance_method : t -> bool

val is_java_access_method : t -> bool

val is_java_class_initializer : t -> bool

val is_java_anonymous_inner_class_method : t -> bool

val is_java_autogen_method : t -> bool

val is_objc_method : t -> bool
(** Includes specialized objective-c methods*)

val is_objc_instance_method : t -> bool
(** Includes specialized objective-c instance methods*)

val is_objc_class_method : t -> bool
(** Includes specialized objective-c class methods*)

val is_objc_nsobject_class : t -> bool

val get_objc_class_name : t -> string option

val is_std_move : t -> bool

val is_shared_ptr_observer : t -> bool
(** Check if it is C++ shared pointer observer, e.g. [std::shared_ptr::operator*] *)

module Comparable : Comparable.S with type t := t

include module type of struct
  include Comparable
end

(** Hash tables with proc names as keys. *)
module Hash : Caml.Hashtbl.S with type key = t

module LRUHash : LRUHashtbl.S with type key = t

module HashQueue : Hash_queue.S with type key = t

module HashSet : HashSet.S with type elt = t

(** Maps from proc names. *)
module Map : PrettyPrintable.PPMap with type key = t

(** Sets of proc names. *)
module Set : PrettyPrintable.PPSet with type elt = t

module SQLite : SqliteUtils.Data with type t = t

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
  -> t
(** Create a Java procedure name. *)

val make_csharp :
     class_name:Typ.Name.t
  -> return_type:Typ.t option
  -> method_name:string
  -> parameters:Typ.t list
  -> kind:CSharp.kind
  -> t
(** Create a CSharp procedure name. *)

val make_erlang : module_name:string -> function_name:string -> arity:int -> t
(** Create an Erlang procedure name. *)

val make_hack : class_name:HackClassName.t option -> function_name:string -> arity:int option -> t
(** Create a Hack procedure name. *)

val make_objc_dealloc : Typ.Name.t -> t
(** Create a Objective-C dealloc name. This is a destructor for an Objective-C class. This procname
    is given by the class name, since it is always an instance method with the name "dealloc" *)

val make_objc_copy : Typ.Name.t -> t
(** Create a Objective-C copy name. *)

val make_objc_copyWithZone : is_mutable:bool -> Typ.Name.t -> t
(** Create an Objective-C method for copyWithZone: or mutableCopyWithZone: according to is_mutable. *)

val make_python :
  class_name:PythonClassName.t option -> function_name:string -> arity:int option -> t
(** Create a Python procedure name. *)

val empty_block : t
(** Empty block name. *)

val get_language : t -> Language.t
(** Return the language of the procedure. *)

val get_method : t -> string
(** Return the method/function of a procname. *)

val is_objc_block : t -> bool
(** Return whether the procname is a block procname. *)

val is_cpp_lambda : t -> bool
(** Return whether the procname is a cpp lambda procname. *)

val is_objc_dealloc : t -> bool
(** Return whether the dealloc method of an Objective-C class. *)

val is_objc_init : t -> bool
(** Return whether the init method of an Objective-C class. *)

val is_c_method : t -> bool
(** Return true this is an Objective-C/C++ method name. *)

val is_clang : t -> bool
(** Return true this is an Objective-C/C++ method name or a C function. *)

val is_constructor : t -> bool
(** Check if this is a constructor. *)

val is_csharp : t -> bool
(** Check if this is a CSharp procedure name. *)

val is_hack : t -> bool
(** Check if this is a Hack procedure name. *)

val is_java : t -> bool
(** Check if this is a Java procedure name. *)

val is_python : t -> bool
(** Check if this is a Python procedure name. *)

val objc_cpp_replace_method_name : t -> string -> t

val is_infer_undefined : t -> bool
(** Check if this is a special Infer undefined procedure. *)

val is_static : t -> bool option
(** Check if a procedure is a static class method or not. If the procedure is not a class method or
    is unknown to be static, it returns [None]. For now, this checking does not work on C++ methods. *)

val get_global_name_of_initializer : t -> string option
(** Return the name of the global for which this procedure is the initializer if this is an
    initializer, None otherwise. *)

val pp_without_templates : Format.formatter -> t -> unit
(** Pretty print a c++ proc name for the user to see. *)

val pp : Format.formatter -> t -> unit
(** Pretty print a proc name for the user to see. *)

val pp_verbose : Format.formatter -> t -> unit
(** Pretty print a proc name for the user to see with verbosity parameter. *)

val to_string : ?verbosity:detail_level -> t -> string
(** Convert a proc name into a string for the user to see. *)

val to_string_verbose : t -> string
(** Convert a proc name into a string for the user to see with verbosity parameter. *)

val describe : Format.formatter -> t -> unit
(** to use in user messages *)

val replace_class : t -> ?arity_incr:int -> Typ.Name.t -> t
(** Replace the class name component of a procedure name. In case of Java, replace package and class
    name. *)

val is_method_in_objc_protocol : t -> bool

val pp_simplified_string : ?withclass:bool -> F.formatter -> t -> unit
(** Pretty print a proc name as an easy string for the user to see in an IDE. *)

val to_simplified_string : ?withclass:bool -> t -> string
(** Convert a proc name into an easy string for the user to see in an IDE. *)

val from_string_c_fun : string -> t
(** Convert a string to a c function name. *)

val replace_java_inner_class_prefix_regex : string -> string
(** Replace "$\[0-9\]+" index into "$_" in Java proc name. *)

val hashable_name : t -> string
(** Convert the procedure name in a format suitable for computing the bug hash. *)

val pp_unique_id : F.formatter -> t -> unit
(** Print a proc name as a unique identifier. *)

val to_unique_id : t -> string
(** Convert a proc name into a unique identifier. *)

val to_short_unique_name : t -> string
(** Convert a proc name into a unique identfier guaranteed to be short (less than 50 characters) *)

val to_filename : t -> string
(** Convert a proc name to a filename. *)

val get_qualifiers : t -> QualifiedCppName.t
(** get qualifiers of C/objc/C++ method/function *)

val decr_hack_arity : t -> t option
(** return a Hack procname with decremented arity. Return None if input has no arity or 0 arity *)

val get_hack_arity : t -> int option
(** get the arity of a Hack procname *)

val get_hack_static_init : is_trait:bool -> HackClassName.t -> t
(** get the sinit procname in Hack *)

val pp_name_only : F.formatter -> t -> unit
(** Print name of procedure with at most one-level path. For example,

    - In C++: "<ClassName>::<ProcName>"
    - In Java, ObjC, C#: "<ClassName>.<ProcName>"
    - In C: "<ProcName>"
    - In Erlang: "<ModuleName>:<ProcName>" *)

val pp_fullname_only : F.formatter -> t -> unit
(** Like [pp_name_only], but include package name for Java. *)

val is_c : t -> bool

val is_lambda_name : string -> bool

val is_lambda : t -> bool

val is_lambda_or_block : t -> bool

val patterns_match : Str.regexp list -> t -> bool
(** Test whether a proc name matches to one of the regular expressions. *)

val is_erlang_unsupported : t -> bool

val is_erlang : t -> bool

val erlang_call_unqualified : arity:int -> t
(** A special infer-erlang procname that represents a syntactic erlang (unqualified) function call.
    [arity] is the arity of the erlang function. First parameter of this procedure is expecteed to
    be the erlang function name, and the remaining parameters are the erlang parameters (given
    one-by-one and not as an erlang list). *)

val erlang_call_qualified : arity:int -> t
(** Same as [erlang_call_unqualified] but is expected to have an erlang module name as the first
    parameter, and the function name as second. [arity] is (still) the erlang arity of the function. *)

val is_erlang_call_unqualified : t -> bool

val is_erlang_call_qualified : t -> bool

val is_hack_builtins : t -> bool

val is_hack_sinit : t -> bool

val has_hack_classname : t -> bool

val is_hack_async_name : t -> bool
(* Checks if the function name starts with "gen", which is a (lint-checked) convention for it being async at Meta *)

val is_hack_construct : t -> bool
