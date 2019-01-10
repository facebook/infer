(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The Smallfoot Intermediate Language: Types *)

open! IStd
module F = Format

module IntegerWidths : sig
  type t = {char_width: int; short_width: int; int_width: int; long_width: int; longlong_width: int}
  [@@deriving compare]

  val java : t

  val load : SourceFile.t -> t option

  module SQLite : sig
    val serialize : t option -> Sqlite3.Data.t
  end
end

(** Kinds of integers *)
type ikind =
  | IChar  (** [char] *)
  | ISChar  (** [signed char] *)
  | IUChar  (** [unsigned char] *)
  | IBool  (** [bool] *)
  | IInt  (** [int] *)
  | IUInt  (** [unsigned int] *)
  | IShort  (** [short] *)
  | IUShort  (** [unsigned short] *)
  | ILong  (** [long] *)
  | IULong  (** [unsigned long] *)
  | ILongLong  (** [long long] (or [_int64] on Microsoft Visual C) *)
  | IULongLong  (** [unsigned long long] (or [unsigned _int64] on Microsoft Visual C) *)
  | I128  (** [__int128_t] *)
  | IU128  (** [__uint128_t] *)
[@@deriving compare]

val equal_ikind : ikind -> ikind -> bool

val width_of_ikind : IntegerWidths.t -> ikind -> int

val range_of_ikind : IntegerWidths.t -> ikind -> Z.t * Z.t

val ikind_is_char : ikind -> bool
(** Check whether the integer kind is a char *)

val ikind_is_unsigned : ikind -> bool
(** Check whether the integer kind is unsigned *)

(** Kinds of floating-point numbers *)
type fkind =
  | FFloat  (** [float] *)
  | FDouble  (** [double] *)
  | FLongDouble  (** [long double] *)
[@@deriving compare]

(** kind of pointer *)
type ptr_kind =
  | Pk_pointer  (** C/C++, Java, Objc standard/__strong pointer *)
  | Pk_reference  (** C++ reference *)
  | Pk_objc_weak  (** Obj-C __weak pointer *)
  | Pk_objc_unsafe_unretained  (** Obj-C __unsafe_unretained pointer *)
  | Pk_objc_autoreleasing  (** Obj-C __autoreleasing pointer *)
[@@deriving compare]

val equal_ptr_kind : ptr_kind -> ptr_kind -> bool

type type_quals [@@deriving compare]

val mk_type_quals :
     ?default:type_quals
  -> ?is_const:bool
  -> ?is_restrict:bool
  -> ?is_volatile:bool
  -> unit
  -> type_quals

val is_const : type_quals -> bool

val is_restrict : type_quals -> bool

val is_volatile : type_quals -> bool

(** types for sil (structured) expressions *)
type t = {desc: desc; quals: type_quals} [@@deriving compare]

and desc =
  | Tint of ikind  (** integer type *)
  | Tfloat of fkind  (** float type *)
  | Tvoid  (** void type *)
  | Tfun of {no_return: bool}  (** function type with noreturn attribute *)
  | Tptr of t * ptr_kind  (** pointer type *)
  | Tstruct of name  (** structured value type name *)
  | TVar of string  (** type variable (ie. C++ template variables) *)
  | Tarray of {elt: t; length: IntLit.t option; stride: IntLit.t option}
      (** array type with statically fixed length and stride *)
[@@deriving compare]

and name =
  | CStruct of QualifiedCppName.t
  | CUnion of QualifiedCppName.t
  (* qualified name does NOT contain template arguments of the class. It will contain template
     args of its parent classes, for example: MyClass<int>::InnerClass<int> will store
     "MyClass<int>", "InnerClass" *)
  | CppClass of QualifiedCppName.t * template_spec_info
  | JavaClass of Mangled.t
  | ObjcClass of QualifiedCppName.t
  | ObjcProtocol of QualifiedCppName.t
[@@deriving compare]

and template_arg = TType of t | TInt of Int64.t | TNull | TNullPtr | TOpaque [@@deriving compare]

and template_spec_info =
  | NoTemplate
  | Template of
      { mangled: string option
            (** WARNING: because of type substitutions performed by [sub_type] and [sub_tname],
                mangling is not guaranteed to be unique to a single type. All the information in
                the template arguments is also needed for uniqueness. *)
      ; args: template_arg list }
[@@deriving compare]

val mk : ?default:t -> ?quals:type_quals -> desc -> t
(** Create Typ.t from given desc. if [default] is passed then use its value to set other fields such as quals *)

val mk_array : ?default:t -> ?quals:type_quals -> ?length:IntLit.t -> ?stride:IntLit.t -> t -> t
(** Create an array type from a given element type. If [length] or [stride] value is given, use them as static length and size. *)

val void : t
(** void type *)

val void_star : t
(** void* type *)

val get_ikind_opt : t -> ikind option
(** Get ikind if the type is integer. *)

val size_t : ikind
(** ikind of size_t *)

module Name : sig
  (** Named types. *)
  type t = name [@@deriving compare]

  val equal : t -> t -> bool
  (** Equality for typenames *)

  val to_string : t -> string
  (** convert the typename to a string *)

  val pp : Format.formatter -> t -> unit

  val is_class : t -> bool
  (** [is_class name] holds if [name] names CPP/Objc/Java class *)

  val is_union : t -> bool
  (** [is_union name] holds if [name] names C/CPP union *)

  val is_same_type : t -> t -> bool
  (** [is_class name1 name2] holds if [name1] and [name2] name same kind of type *)

  val name : t -> string
  (** name of the typename without qualifier *)

  val qual_name : t -> QualifiedCppName.t
  (** qualified name of the type, may return nonsense for Java classes *)

  val unqualified_name : t -> QualifiedCppName.t

  module C : sig
    val from_string : string -> t

    val from_qual_name : QualifiedCppName.t -> t

    val union_from_qual_name : QualifiedCppName.t -> t
  end

  module Java : sig
    module Split : sig
      type t

      val make : ?package:string -> string -> t

      val java_lang_object : t

      val java_lang_string : t

      val package : t -> string option

      val type_name : t -> string
    end

    val from_string : string -> t
    (** Create a typename from a Java classname in the form "package.class" *)

    val from_package_class : string -> string -> t
    (** Create a typename from a package name and a class name *)

    val is_class : t -> bool
    (** [is_class name] holds if [name] names a Java class *)

    val is_external_classname : string -> bool
    (** return true if the string is in the .inferconfig list of external classes *)

    val is_external : t -> bool
    (** return true if the typename is in the .inferconfig list of external classes *)

    val get_outer_class : t -> t option
    (** Given an inner classname like C$Inner1$Inner2, return Some C$Inner1. If the class is not an
        inner class, return None *)

    val java_lang_object : t

    val java_io_serializable : t

    val java_lang_cloneable : t
  end

  module Cpp : sig
    val from_qual_name : template_spec_info -> QualifiedCppName.t -> t
    (** Create a typename from a C++ classname *)

    val is_class : t -> bool
    (** [is_class name] holds if [name] names a C++ class *)
  end

  module Objc : sig
    val from_string : string -> t
    (** Create a typename from a Objc classname *)

    val from_qual_name : QualifiedCppName.t -> t

    val protocol_from_qual_name : QualifiedCppName.t -> t
  end

  module Set : Caml.Set.S with type elt = t
end

val equal : t -> t -> bool
(** Equality for types. *)

val equal_desc : desc -> desc -> bool

val equal_quals : type_quals -> type_quals -> bool

val equal_ignore_quals : t -> t -> bool
(** Equality for types, but ignoring quals in it. *)

val pp_full : Pp.env -> F.formatter -> t -> unit
(** Pretty print a type with all the details. *)

val pp : Pp.env -> F.formatter -> t -> unit
(** Pretty print a type. *)

val to_string : t -> string

val d_full : t -> unit
(** Dump a type with all the details. *)

val d_list : t list -> unit
(** Dump a list of types. *)

val name : t -> Name.t option
(** The name of a type *)

val strip_ptr : t -> t
(** turn a *T into a T. fails if [t] is not a pointer type *)

val array_elem : t option -> t -> t
(** If an array type, return the type of the element.
    If not, return the default type if given, otherwise raise an exception *)

val is_objc_class : t -> bool

val is_cpp_class : t -> bool

val is_pointer_to_cpp_class : t -> bool

val is_pointer_to_void : t -> bool

val is_pointer_to_int : t -> bool

val is_pointer : t -> bool

val is_reference : t -> bool

val is_struct : t -> bool

val is_int : t -> bool

val is_unsigned_int : t -> bool

val has_block_prefix : string -> bool

val unsome : string -> t option -> t

type typ = t

module Procname : sig
  (** Module for Procedure Names. *)

  (** Type of java procedure names. *)
  module Java : sig
    type kind =
      | Non_Static
          (** in Java, procedures called with invokevirtual, invokespecial, and invokeinterface *)
      | Static  (** in Java, procedures called with invokestatic *)

    type t [@@deriving compare]

    type java_type = Name.Java.Split.t

    val constructor_method_name : string

    val class_initializer_method_name : string

    val compare_java_type : java_type -> java_type -> int

    val make : Name.t -> java_type option -> string -> java_type list -> kind -> t
    (** Create a Java procedure name from its
        class_name method_name args_type_name return_type_name method_kind. *)

    val replace_method_name : string -> t -> t
    (** Replace the method name of an existing java procname. *)

    val replace_parameters : java_type list -> t -> t
    (** Replace the parameters of a java procname. *)

    val replace_return_type : java_type -> t -> t
    (** Replace the method of a java procname. *)

    val get_class_name : t -> string
    (** Return the class name of a java procedure name. *)

    val get_class_type_name : t -> Name.t
    (** Return the class name as a typename of a java procedure name. *)

    val get_simple_class_name : t -> string
    (** Return the simple class name of a java procedure name. *)

    val get_package : t -> string option
    (** Return the package name of a java procedure name. *)

    val get_method : t -> string
    (** Return the method name of a java procedure name. *)

    val get_parameters : t -> java_type list
    (** Return the parameters of a java procedure name. *)

    val get_return_typ : t -> typ
    (** Return the return type of [pname_java]. return Tvoid if there's no return type *)

    val is_access_method : t -> bool
    (** Check if the procedure name is an acess method (e.g. access$100 used to
          access private members from a nested class. *)

    val is_autogen_method : t -> bool
    (** Check if the procedure name is of an auto-generated method containing '$'. *)

    val is_anonymous_inner_class_constructor : t -> bool
    (** Check if the procedure name is an anonymous inner class constructor. *)

    val is_close : t -> bool
    (** Check if the method name is "close". *)

    val is_static : t -> bool
    (** Check if the java procedure is static. *)

    val is_vararg : t -> bool
    (** Check if the proc name has the type of a java vararg.
          Note: currently only checks that the last argument has type Object[]. *)

    val is_lambda : t -> bool
    (** Check if the proc name comes from a lambda expression *)

    val is_generated : t -> bool
    (** Check if the proc name comes from generated code *)

    val is_class_initializer : t -> bool
    (** Check if this is a class initializer. *)

    val is_external : t -> bool
    (** Check if the method belongs to one of the specified external packages *)
  end

  module Parameter : sig
    (** Type for parameters in clang procnames, [Some name] means the parameter is of type pointer to struct, with [name]
being the name of the struct, [None] means the parameter is of some other type. *)
    type clang_parameter = Name.t option [@@deriving compare]

    (** Type for parameters in procnames, for java and clang. *)
    type t = JavaParameter of Java.java_type | ClangParameter of clang_parameter
    [@@deriving compare]

    val of_typ : typ -> clang_parameter
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
      { class_name: Name.t
      ; kind: kind
      ; method_name: string
      ; parameters: Parameter.clang_parameter list
      ; template_args: template_spec_info }
    [@@deriving compare]

    val make :
      Name.t -> string -> kind -> template_spec_info -> Parameter.clang_parameter list -> t
    (** Create an objc procedure name from a class_name and method_name. *)

    val get_class_name : t -> string

    val get_class_type_name : t -> Name.t [@@warning "-32"]

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

    val is_operator_equal : t -> bool
    (** Return true if the procname is operator= *)
  end

  module C : sig
    (** Type of c procedure names. *)
    type t = private
      { name: QualifiedCppName.t
      ; mangled: string option
      ; parameters: Parameter.clang_parameter list
      ; template_args: template_spec_info }

    val c :
      QualifiedCppName.t -> string -> Parameter.clang_parameter list -> template_spec_info -> t
    (** Create a C procedure name from plain and mangled name. *)
  end

  module Block : sig
    (** Type of Objective C block names. *)
    type block_name = string

    type t = {name: block_name; parameters: Parameter.clang_parameter list} [@@deriving compare]

    val make : block_name -> Parameter.clang_parameter list -> t
  end

  (** Type of procedure names.
  WithBlockParameters is used for creating an instantiation of a method that contains block parameters
  and it's called with concrete blocks. For example:
  foo(Block block) {block();}
  bar() {foo(my_block)} is executed as  foo_my_block() {my_block(); }
  where foo_my_block is created with WithBlockParameters (foo, [my_block]) *)
  type t =
    | Java of Java.t
    | C of C.t
    | Linters_dummy_method
    | Block of Block.t
    | ObjC_Cpp of ObjC_Cpp.t
    | WithBlockParameters of t * Block.block_name list
  [@@deriving compare]

  val block_name_of_procname : t -> Block.block_name

  val equal : t -> t -> bool

  val get_class_type_name : t -> Name.t option

  val get_class_name : t -> string option

  val get_parameters : t -> Parameter.t list

  val replace_parameters : Parameter.t list -> t -> t

  val parameter_of_name : t -> Name.t -> Parameter.t

  val is_objc_method : t -> bool

  (** Hash tables with proc names as keys. *)
  module Hash : Caml.Hashtbl.S with type key = t

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

  val empty_block : t
  (** Empty block name. *)

  val get_language : t -> Language.t
  (** Return the language of the procedure. *)

  val get_method : t -> string
  (** Return the method/function of a procname. *)

  val is_objc_block : t -> bool
  (** Return whether the procname is a block procname. *)

  val hash_pname : t -> int
  (** Hash function for procname. *)

  val is_c_method : t -> bool
  (** Return true this is an Objective-C/C++ method name. *)

  val is_clang : t -> bool
  (** Return true if this is a C, C++, or Objective-C procedure name *)

  val is_constructor : t -> bool
  (** Check if this is a constructor. *)

  val is_java : t -> bool
  (** Check if this is a Java procedure name. *)

  val with_block_parameters : t -> Block.block_name list -> t
  (** Create a procedure name instantiated with block parameters from a base procedure name
    and a list of block procedure names (the arguments). *)

  val objc_cpp_replace_method_name : t -> string -> t

  val is_infer_undefined : t -> bool
  (** Check if this is a special Infer undefined procedure. *)

  val get_global_name_of_initializer : t -> string option
  (** Return the name of the global for which this procedure is the initializer if this is an
      initializer, None otherwise. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty print a proc name. *)

  val replace_class : t -> Name.t -> t
  (** Replace the class name component of a procedure name.
      In case of Java, replace package and class name. *)

  val is_method_in_objc_protocol : t -> bool

  val to_string : t -> string
  (** Convert a proc name to a string for the user to see. *)

  val to_simplified_string : ?withclass:bool -> t -> string
  (** Convert a proc name into a easy string for the user to see in an IDE. *)

  val from_string_c_fun : string -> t
  (** Convert a string to a c function name. *)

  val hashable_name : t -> string
  (** Print the procedure name in a format suitable for computing the bug hash *)

  val to_unique_id : t -> string
  (** Convert a proc name into a unique identifier. *)

  val to_filename : ?crc_only:bool -> t -> string
  (** Convert a proc name to a filename or only to its crc. *)

  val get_qualifiers : t -> QualifiedCppName.t
  (** get qualifiers of C/objc/C++ method/function *)
end

module Fieldname : sig
  (** Names for fields of class/struct/union *)
  type t [@@deriving compare]

  val equal : t -> t -> bool
  (** Equality for field names. *)

  (** Set for fieldnames *)

  module Set : Caml.Set.S with type elt = t

  (** Map for fieldnames *)

  module Map : Caml.Map.S with type key = t

  module Clang : sig
    val from_class_name : Name.t -> string -> t
  end

  module Java : sig
    val from_string : string -> t
    (** Create a java field name from string *)

    val is_captured_parameter : t -> bool
    (** Check if field is a captured parameter *)

    val get_class : t -> string
    (** The class part of the fieldname *)

    val get_field : t -> string
    (** The last component of the fieldname *)

    val is_outer_instance : t -> bool
    (** Check if the field is the synthetic this$n of a nested class, used to access the n-th outer
        instance. *)
  end

  val to_string : t -> string
  (** Convert a field name to a string. *)

  val to_full_string : t -> string

  val to_simplified_string : t -> string
  (** Convert a fieldname to a simplified string with at most one-level path. *)

  val to_flat_string : t -> string
  (** Convert a fieldname to a flat string without path. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty print a field name. *)

  val clang_get_qual_class : t -> QualifiedCppName.t option
  (** get qualified classname of a field if it's coming from clang frontend. returns None otherwise *)
end

module Struct : sig
  type field = Fieldname.t * typ * Annot.Item.t [@@deriving compare]

  type fields = field list

  (** Type for a structured value. *)
  type t = private
    { fields: fields  (** non-static fields *)
    ; statics: fields  (** static fields *)
    ; supers: Name.t list  (** supers *)
    ; methods: Procname.t list  (** methods defined *)
    ; exported_objc_methods: Procname.t list  (** methods in ObjC interface, subset of [methods] *)
    ; annots: Annot.Item.t  (** annotations *) }

  type lookup = Name.t -> t option

  val pp_field : Pp.env -> F.formatter -> field -> unit

  val pp : Pp.env -> Name.t -> F.formatter -> t -> unit
  (** Pretty print a struct type. *)

  val internal_mk_struct :
       ?default:t
    -> ?fields:fields
    -> ?statics:fields
    -> ?methods:Procname.t list
    -> ?exported_objc_methods:Procname.t list
    -> ?supers:Name.t list
    -> ?annots:Annot.Item.t
    -> unit
    -> t
  (** Construct a struct_typ, normalizing field types *)

  val get_extensible_array_element_typ : lookup:lookup -> typ -> typ option
  (** the element typ of the final extensible array in the given typ, if any *)

  val fld_typ : lookup:lookup -> default:typ -> Fieldname.t -> typ -> typ
  (** If a struct type with field f, return the type of f.
      If not, return the default type if given, otherwise raise an exception *)

  val get_field_type_and_annotation :
    lookup:lookup -> Fieldname.t -> typ -> (typ * Annot.Item.t) option
  (** Return the type of the field [fn] and its annotation, None if [typ] has no field named [fn] *)
end
