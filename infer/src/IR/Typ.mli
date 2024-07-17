(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The Smallfoot Intermediate Language: Types *)

open! IStd
module F = Format

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
[@@deriving compare, equal, hash, normalize]

val ikind_is_char : ikind -> bool
(** Check whether the integer kind is a char *)

val ikind_is_unsigned : ikind -> bool
(** Check whether the integer kind is unsigned *)

(** Kinds of floating-point numbers *)
type fkind = FFloat  (** [float] *) | FDouble  (** [double] *) | FLongDouble  (** [long double] *)
[@@deriving compare]

(** kind of pointer *)
type ptr_kind =
  | Pk_pointer  (** C/C++, Java, Objc standard/__strong pointer *)
  | Pk_lvalue_reference  (** C++ lvalue reference *)
  | Pk_rvalue_reference  (** C++ rvalue reference *)
  | Pk_objc_weak  (** Obj-C __weak pointer *)
  | Pk_objc_unsafe_unretained  (** Obj-C __unsafe_unretained pointer *)
  | Pk_objc_autoreleasing  (** Obj-C __autoreleasing pointer *)
[@@deriving compare]

val equal_ptr_kind : ptr_kind -> ptr_kind -> bool

type type_quals [@@deriving compare, equal]

val mk_type_quals :
     ?default:type_quals
  -> ?is_const:bool
  -> ?is_reference:bool
  -> ?is_restrict:bool
  -> ?is_volatile:bool
  -> unit
  -> type_quals

val is_const : type_quals -> bool

val is_restrict : type_quals -> bool

val is_volatile : type_quals -> bool

(** types for sil (structured) expressions *)
type t = {desc: desc; quals: type_quals}

and desc =
  | Tint of ikind  (** integer type *)
  | Tfloat of fkind  (** float type *)
  | Tvoid  (** void type *)
  | Tfun  (** function type *)
  | Tptr of t * ptr_kind  (** pointer type *)
  | Tstruct of name  (** structured value type name *)
  | TVar of string  (** type variable (ie. C++ template variables) *)
  | Tarray of {elt: t; length: IntLit.t option; stride: IntLit.t option}
      (** array type with statically fixed length and stride *)

and objc_block_sig = {class_name: name option; name: string; mangled: string}
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

and c_function_sig =
  {c_name: QualifiedCppName.t; c_mangled: string option; c_template_args: template_spec_info}
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

and name =
  | CStruct of QualifiedCppName.t
  | CUnion of QualifiedCppName.t
  | CppClass of
      {name: QualifiedCppName.t; template_spec_info: template_spec_info; is_union: bool [@ignore]}
  | CSharpClass of CSharpClassName.t
  | ErlangType of ErlangTypeName.t
  | HackClass of HackClassName.t
  | JavaClass of JavaClassName.t
  | ObjcClass of QualifiedCppName.t
  | ObjcProtocol of QualifiedCppName.t
  | PythonClass of PythonClassName.t
  | ObjcBlock of objc_block_sig
  | CFunction of c_function_sig
[@@deriving hash, sexp]

and template_arg = TType of t | TInt of Int64.t | TNull | TNullPtr | TOpaque

and template_spec_info =
  | NoTemplate
  | Template of
      { mangled: string option
            (** WARNING: because of type substitutions performed by [sub_type] and [sub_tname],
                mangling is not guaranteed to be unique to a single type. All the information in the
                template arguments is also needed for uniqueness. *)
      ; args: template_arg list }

val pp_template_spec_info : Pp.env -> F.formatter -> template_spec_info -> unit
[@@warning "-unused-value-declaration"]

val is_template_spec_info_empty : template_spec_info -> bool

val mk : ?default:t -> ?quals:type_quals -> desc -> t
(** Create Typ.t from given desc. if [default] is passed then use its value to set other fields such
    as quals *)

val mk_array : ?default:t -> ?quals:type_quals -> ?length:IntLit.t -> ?stride:IntLit.t -> t -> t
(** Create an array type from a given element type. If [length] or [stride] value is given, use them
    as static length and size. *)

val mk_struct : name -> t

val mk_ptr : ?ptr_kind:ptr_kind -> t -> t
(** make a pointer to [t], default kind is [Pk_pointer] *)

val set_ptr_to_const : t -> t

val set_to_const : t -> t

val get_ikind_opt : t -> ikind option
(** Get ikind if the type is integer. *)

val size_t : ikind
(** ikind of size_t *)

val is_weak_pointer : t -> bool

val is_strong_pointer : t -> bool

module Name : sig
  (** Named types. *)
  type t = name [@@deriving compare, yojson_of, sexp, hash, normalize]

  val compare_name : t -> t -> int
  (** Similar to compare, but compares only names, except template arguments. *)

  val equal : t -> t -> bool
  (** Equality for typenames *)

  val hash : t -> int

  val to_string : t -> string
  (** convert the typename to a string *)

  val pp : Format.formatter -> t -> unit

  val is_class : t -> bool
  (** [is_class name] holds if [name] names CPP/Objc/Java class *)

  val is_union : t -> bool
  (** [is_union name] holds if [name] names C/CPP union *)

  val is_same_type : t -> t -> bool
  (** [is_class name1 name2] holds if [name1] and [name2] name same kind of type *)

  val name_without_templates : t -> string
  (** name of the c++ typename without qualifier *)

  val name : t -> string
  (** name of the typename *)

  val qual_name : t -> QualifiedCppName.t
  (** qualified name of the type, may return nonsense for Java classes *)

  val unqualified_name : t -> QualifiedCppName.t

  val get_template_spec_info : t -> template_spec_info option

  val is_objc_protocol : t -> bool

  val is_objc_class : t -> bool

  val is_objc_block : t -> bool

  val is_hack_class : t -> bool

  val is_python_class : t -> bool

  module C : sig
    val from_string : string -> t

    val from_qual_name : QualifiedCppName.t -> t

    val union_from_qual_name : QualifiedCppName.t -> t
  end

  module CSharp : sig
    val from_string : string -> t

    val is_class : t -> bool
  end

  module Hack : sig
    val static_companion : t -> t
    (** See {!HackClassName.static_companion} *)

    val static_companion_origin : t -> t
    (** See {!HackClassName.static_companion_origin} *)

    val is_static_companion : t -> bool
    (** See {!HackClassName.is_static_companion} *)

    val is_generated_curry : t -> bool
    (** See {!HackClassName.is_generated_curry} *)

    val extract_curry_info : t -> (HackClassName.t * string) option
    (** See {!HackClassName.extract_curry_info} *)
  end

  module Java : sig
    val from_string : string -> t
    (** Create a typename from a Java classname in the form "package.class" *)

    val is_class : t -> bool
    (** [is_class name] holds if [name] names a Java class *)

    val get_java_class_name_opt : t -> JavaClassName.t option
    (** Return underlying JavaClassName if [name] is java class *)

    val get_java_class_name_exn : t -> JavaClassName.t
    (** Ensure [name] is a java class name and return underlying JavaClassName *)

    val is_external : t -> bool
    (** return true if the typename is in the .inferconfig list of external classes *)

    val is_anonymous_inner_class_name_opt : t -> bool option
    (** return None if it is not a Java class *)
  end

  module Cpp : sig
    val from_qual_name : template_spec_info -> is_union:bool -> QualifiedCppName.t -> t
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

  module Set : PrettyPrintable.PPSet with type elt = t

  module Map : PrettyPrintable.PPMap with type key = t

  module Hash : Caml.Hashtbl.S with type key = t
end

val equal : t -> t -> bool
(** Equality for types. *)

val equal_desc : desc -> desc -> bool

val equal_name : name -> name -> bool

val equal_ignore_quals : t -> t -> bool
(** Equality for types, but ignoring quals in it. *)

val overloading_resolution : (t -> t -> bool) list
(** [overloading_resolution] is a list of predicates that compare whether a type T1 binds a type T2. *)

val pp_full : Pp.env -> F.formatter -> t -> unit
(** Pretty print a type with all the details. *)

val pp : Pp.env -> F.formatter -> t -> unit
(** Pretty print a type. *)

val pp_desc : Pp.env -> F.formatter -> desc -> unit
(** Pretty print a type desc. *)

val pp_java : verbose:bool -> F.formatter -> t -> unit
(** Pretty print a Java type. Raises if type isn't produced by the Java frontend *)

val pp_cs : verbose:bool -> F.formatter -> t -> unit
(** Pretty print a Java type. Raises if type isn't produced by the CSharp frontend *)

val to_string : t -> string

val desc_to_string : desc -> string

val d_full : t -> unit
(** Dump a type with all the details. *)

val d_list : t list -> unit
(** Dump a list of types. *)

val name : t -> Name.t option
(** The name of a type *)

val strip_ptr : t -> t
(** turn a *T into a T. fails if [t] is not a pointer type *)

val is_ptr_to_ignore_quals : t -> ptr:t -> bool
(** check if [ptr] is a pointer type to [t], ignoring quals *)

val is_ptr_to_const : t -> bool
(** check if typ is a pointer type to const *)

val array_elem : t option -> t -> t
(** If an array type, return the type of the element. If not, return the default type if given,
    otherwise raise an exception *)

val is_objc_class : t -> bool

val is_cpp_class : t -> bool

val is_pointer_to_cpp_class : t -> bool

val is_pointer_to_smart_pointer : t -> bool

val is_unique_pointer : t -> bool

val is_pointer_to_unique_pointer : t -> bool

val shared_pointer_matcher : QualifiedCppName.Match.quals_matcher

val is_shared_pointer : t -> bool

val is_folly_coro : t -> bool

val is_pointer_to_void : t -> bool

val is_void : t -> bool

val is_pointer_to_int : t -> bool

val is_pointer_to_const : t -> bool

val is_pointer_to_function : t -> bool

val is_pointer : t -> bool

val is_reference : t -> bool

val is_rvalue_reference : t -> bool

val is_const_reference_on_source : t -> bool

val is_struct : t -> bool

val is_int : t -> bool

val is_unsigned_int : t -> bool

val is_char : t -> bool

val is_csharp_type : t -> bool
(** is [t] a type produced by the Java frontend? *)

val is_java_type : t -> bool
(** is [t] a type produced by the Java frontend? *)

val unsome : string -> t option -> t
