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
type fkind = FFloat  (** [float] *) | FDouble  (** [double] *) | FLongDouble  (** [long double] *)
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
type t = {desc: desc; quals: type_quals} [@@deriving compare, yojson_of]

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

and name =
  | CStruct of QualifiedCppName.t
  | CUnion of QualifiedCppName.t
      (** qualified name does NOT contain template arguments of the class. It will contain template
          args of its parent classes, for example: MyClass<int>::InnerClass<int> will store
          "MyClass<int>", "InnerClass" *)
  | CppClass of {name: QualifiedCppName.t; template_spec_info: template_spec_info; is_union: bool}
  | CSharpClass of CSharpClassName.t
  | JavaClass of JavaClassName.t
  | ObjcClass of QualifiedCppName.t * name list
      (** ObjC class that conforms to a list of protocols, e.g. id<NSFastEnumeration, NSCopying> *)
  | ObjcProtocol of QualifiedCppName.t

and template_arg = TType of t | TInt of Int64.t | TNull | TNullPtr | TOpaque

and template_spec_info =
  | NoTemplate
  | Template of
      { mangled: string option
            (** WARNING: because of type substitutions performed by [sub_type] and [sub_tname],
                mangling is not guaranteed to be unique to a single type. All the information in the
                template arguments is also needed for uniqueness. *)
      ; args: template_arg list }

val pp_template_spec_info : Pp.env -> F.formatter -> template_spec_info -> unit [@@warning "-32"]

val mk : ?default:t -> ?quals:type_quals -> desc -> t
(** Create Typ.t from given desc. if [default] is passed then use its value to set other fields such
    as quals *)

val mk_array : ?default:t -> ?quals:type_quals -> ?length:IntLit.t -> ?stride:IntLit.t -> t -> t
(** Create an array type from a given element type. If [length] or [stride] value is given, use them
    as static length and size. *)

val mk_struct : name -> t

val mk_ptr : ?ptr_kind:ptr_kind -> t -> t
(** make a pointer to [t], default kind is [Pk_pointer] *)

val get_ikind_opt : t -> ikind option
(** Get ikind if the type is integer. *)

val size_t : ikind
(** ikind of size_t *)

val is_weak_pointer : t -> bool

val is_strong_pointer : t -> bool

module Name : sig
  (** Named types. *)
  type t = name [@@deriving compare, yojson_of]

  val loose_compare : t -> t -> int
  (** Similar to compare, but addresses [CStruct x] and [CppClass x] as equal. *)

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

  val name : t -> string
  (** name of the typename without qualifier *)

  val qual_name : t -> QualifiedCppName.t
  (** qualified name of the type, may return nonsense for Java classes *)

  val unqualified_name : t -> QualifiedCppName.t

  val get_template_spec_info : t -> template_spec_info option

  val is_objc_protocol : t -> bool

  module C : sig
    val from_string : string -> t

    val from_qual_name : QualifiedCppName.t -> t

    val union_from_qual_name : QualifiedCppName.t -> t
  end

  module CSharp : sig
    val from_string : string -> t
  end

  module Java : sig
    val from_string : string -> t
    (** Create a typename from a Java classname in the form "package.class" *)

    val is_class : t -> bool
    (** [is_class name] holds if [name] names a Java class *)

    val get_java_class_name_exn : t -> JavaClassName.t
    (** Ensure [name] is a java class name and return underlying JavaClassName *)

    val is_external : t -> bool
    (** return true if the typename is in the .inferconfig list of external classes *)

    val is_anonymous_inner_class_name_exn : t -> bool
    (** Throws if it is not a Java class *)

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

  module Normalizer : HashNormalizer.S with type t = t
end

val equal : t -> t -> bool
(** Equality for types. *)

val equal_desc : desc -> desc -> bool

val equal_name : name -> name -> bool

val equal_quals : type_quals -> type_quals -> bool

val equal_ignore_quals : t -> t -> bool
(** Equality for types, but ignoring quals in it. *)

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

val pp_protocols : Pp.env -> F.formatter -> name list -> unit

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

val array_elem : t option -> t -> t
(** If an array type, return the type of the element. If not, return the default type if given,
    otherwise raise an exception *)

val is_objc_class : t -> bool

val is_cpp_class : t -> bool

val is_pointer_to_cpp_class : t -> bool

val is_pointer_to_objc_non_tagged_class : t -> bool

val is_pointer_to_void : t -> bool

val is_void : t -> bool

val is_pointer_to_int : t -> bool

val is_pointer_to_function : t -> bool

val is_pointer : t -> bool

val is_reference : t -> bool

val is_struct : t -> bool

val is_int : t -> bool

val is_unsigned_int : t -> bool

val is_char : t -> bool

val is_csharp_type : t -> bool
(** is [t] a type produced by the Java frontend? *)

val is_java_primitive_type : t -> bool
(** is [t] a primitive type produced by the Java frontend? *)

val is_java_type : t -> bool
(** is [t] a type produced by the Java frontend? *)

val has_block_prefix : string -> bool

val unsome : string -> t option -> t

module Normalizer : HashNormalizer.S with type t = t
