(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The Smallfoot Intermediate Language: Types *)

open! IStd
module Hashtbl = Caml.Hashtbl
module L = Logging
module F = Format

module IntegerWidths = struct
  type t = {char_width: int; short_width: int; int_width: int; long_width: int; longlong_width: int}
  [@@deriving compare]

  let java = {char_width= 16; short_width= 16; int_width= 32; long_width= 64; longlong_width= 64}

  module SQLite = SqliteUtils.MarshalledNullableDataNOTForComparison (struct
    type nonrec t = t
  end)

  let load_statement =
    ResultsDatabase.register_statement
      "SELECT integer_type_widths FROM source_files WHERE source_file = :k"


  let load source =
    ResultsDatabase.with_registered_statement load_statement ~f:(fun db load_stmt ->
        SourceFile.SQLite.serialize source
        |> Sqlite3.bind load_stmt 1
        |> SqliteUtils.check_result_code db ~log:"load bind source file" ;
        SqliteUtils.result_single_column_option ~finalize:false ~log:"Typ.IntegerWidths.load" db
          load_stmt
        |> Option.bind ~f:SQLite.deserialize )
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
  | IULongLong  (** [unsigned long long] (or [unsigned int64_] on Microsoft Visual C) *)
  | I128  (** [__int128_t] *)
  | IU128  (** [__uint128_t] *)
[@@deriving compare, equal, yojson_of]

let ikind_to_string = function
  | IChar ->
      "char"
  | ISChar ->
      "signed char"
  | IUChar ->
      "unsigned char"
  | IBool ->
      "_Bool"
  | IInt ->
      "int"
  | IUInt ->
      "unsigned int"
  | IShort ->
      "short"
  | IUShort ->
      "unsigned short"
  | ILong ->
      "long"
  | IULong ->
      "unsigned long"
  | ILongLong ->
      "long long"
  | IULongLong ->
      "unsigned long long"
  | I128 ->
      "__int128_t"
  | IU128 ->
      "__uint128_t"


let width_of_ikind {IntegerWidths.char_width; short_width; int_width; long_width; longlong_width} =
  function
  | IBool ->
      8
  | ISChar | IChar | IUChar ->
      char_width
  | IShort | IUShort ->
      short_width
  | IInt | IUInt ->
      int_width
  | ILong | IULong ->
      long_width
  | ILongLong | IULongLong ->
      longlong_width
  | I128 | IU128 ->
      128


let ikind_is_unsigned = function
  | IBool | IUChar | IUShort | IUInt | IULong | IULongLong | IU128 ->
      true
  | ISChar | IChar | IShort | IInt | ILong | ILongLong | I128 ->
      false


let range_of_ikind =
  let range bits ~unsigned =
    if unsigned then Z.(~$0, shift_left ~$1 bits - ~$1)
    else
      let bound = Z.(shift_left ~$1) (bits - 1) in
      Z.(~-bound, bound - ~$1)
  in
  fun integer_widths x ->
    let bits_for_range = match x with IBool -> 1 | _ -> width_of_ikind integer_widths x in
    range bits_for_range ~unsigned:(ikind_is_unsigned x)


let ikind_is_char = function IChar | ISChar | IUChar -> true | _ -> false

(** Kinds of floating-point numbers *)
type fkind = FFloat  (** [float] *) | FDouble  (** [double] *) | FLongDouble  (** [long double] *)
[@@deriving compare, equal, yojson_of]

let fkind_to_string = function
  | FFloat ->
      "float"
  | FDouble ->
      "double"
  | FLongDouble ->
      "long double"


(** kind of pointer *)
type ptr_kind =
  | Pk_pointer  (** C/C++, Java, Objc standard/__strong pointer *)
  | Pk_reference  (** C++ reference *)
  | Pk_objc_weak  (** Obj-C __weak pointer *)
  | Pk_objc_unsafe_unretained  (** Obj-C __unsafe_unretained pointer *)
  | Pk_objc_autoreleasing  (** Obj-C __autoreleasing pointer *)
[@@deriving compare, equal, yojson_of]

let ptr_kind_string = function
  | Pk_reference ->
      "&"
  | Pk_pointer ->
      "*"
  | Pk_objc_weak ->
      "__weak *"
  | Pk_objc_unsafe_unretained ->
      "__unsafe_unretained *"
  | Pk_objc_autoreleasing ->
      "__autoreleasing *"


module T = struct
  type type_quals = {is_const: bool; is_restrict: bool; is_volatile: bool}
  [@@deriving compare, equal, yojson_of]

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

  and name =
    | CStruct of QualifiedCppName.t
    | CUnion of QualifiedCppName.t
    | CppClass of
        { name: QualifiedCppName.t
        ; template_spec_info: template_spec_info
        ; is_union: bool [@compare.ignore] }
    | CSharpClass of CSharpClassName.t
    | JavaClass of JavaClassName.t
    | ObjcClass of QualifiedCppName.t * name list
    | ObjcProtocol of QualifiedCppName.t

  and template_arg = TType of t | TInt of int64 | TNull | TNullPtr | TOpaque

  and template_spec_info =
    | NoTemplate
    | Template of {mangled: string option; args: template_arg list}
  [@@deriving compare, yojson_of]

  let yojson_of_name = [%yojson_of: _]

  let equal_desc = [%compare.equal: desc]

  let equal_name = [%compare.equal: name]

  let equal_quals = [%compare.equal: type_quals]

  let equal = [%compare.equal: t]

  let rec equal_ignore_quals t1 t2 = equal_desc_ignore_quals t1.desc t2.desc

  and equal_desc_ignore_quals d1 d2 =
    match (d1, d2) with
    | Tint ikind1, Tint ikind2 ->
        equal_ikind ikind1 ikind2
    | Tfloat fkind1, Tfloat fkind2 ->
        equal_fkind fkind1 fkind2
    | Tvoid, Tvoid | Tfun, Tfun ->
        true
    | Tptr (t1, ptr_kind1), Tptr (t2, ptr_kind2) ->
        equal_ptr_kind ptr_kind1 ptr_kind2 && equal_ignore_quals t1 t2
    | Tstruct name1, Tstruct name2 ->
        equal_name name1 name2
    | TVar s1, TVar s2 ->
        String.equal s1 s2
    | Tarray {elt= t1}, Tarray {elt= t2} ->
        equal_ignore_quals t1 t2
    | _, _ ->
        false
end

include T

let mk_type_quals ?default ?is_const ?is_restrict ?is_volatile () =
  let default_ = {is_const= false; is_restrict= false; is_volatile= false} in
  let mk_aux ?(default = default_) ?(is_const = default.is_const)
      ?(is_restrict = default.is_restrict) ?(is_volatile = default.is_volatile) () =
    {is_const; is_restrict; is_volatile}
  in
  mk_aux ?default ?is_const ?is_restrict ?is_volatile ()


let is_const {is_const} = is_const

let is_restrict {is_restrict} = is_restrict

let is_volatile {is_volatile} = is_volatile

let is_weak_pointer t = match t.desc with Tptr (_, Pk_objc_weak) -> true | _ -> false

let is_strong_pointer t = match t.desc with Tptr (_, Pk_pointer) -> true | _ -> false

let mk ?default ?quals desc : t =
  let default_ = {desc; quals= mk_type_quals ()} in
  let mk_aux ?(default = default_) ?(quals = default.quals) desc = {desc; quals} in
  mk_aux ?default ?quals desc


let mk_array ?default ?quals ?length ?stride elt : t =
  mk ?default ?quals (Tarray {elt; length; stride})


let mk_struct name = mk (Tstruct name)

let mk_ptr ?(ptr_kind = Pk_pointer) t = mk (Tptr (t, ptr_kind))

let get_ikind_opt {desc} = match desc with Tint ikind -> Some ikind | _ -> None

(* TODO: size_t should be implementation-dependent. *)
let size_t = IULong

let escape pe = if Pp.equal_print_kind pe.Pp.kind Pp.HTML then Escape.escape_xml else ident

(** Pretty print a type with all the details, using the C syntax. *)
let rec pp_full pe f typ =
  let pp_quals f {quals} =
    if is_const quals then F.pp_print_string f " const " ;
    if is_restrict quals then F.pp_print_string f " __restrict " ;
    if is_volatile quals then F.pp_print_string f " volatile "
  in
  F.fprintf f "%a%a" (pp_desc pe) typ.desc pp_quals typ


and pp_desc pe f desc =
  match desc with
  | Tstruct tname ->
      (pp_name_c_syntax pe) f tname
  | TVar name ->
      F.pp_print_string f name
  | Tint ik ->
      F.pp_print_string f (ikind_to_string ik)
  | Tfloat fk ->
      F.pp_print_string f (fkind_to_string fk)
  | Tvoid ->
      F.pp_print_string f "void"
  | Tfun ->
      F.pp_print_string f "_fn_"
  | Tptr (({desc= Tarray _ | Tfun} as typ), pk) ->
      F.fprintf f "%a(%s)" (pp_full pe) typ (ptr_kind_string pk |> escape pe)
  | Tptr (typ, pk) ->
      F.fprintf f "%a%s" (pp_full pe) typ (ptr_kind_string pk |> escape pe)
  | Tarray {elt; length; stride} ->
      let pp_int_opt fmt = function Some x -> IntLit.pp fmt x | None -> F.pp_print_char fmt '_' in
      F.fprintf f "%a[%a*%a]" (pp_full pe) elt pp_int_opt length pp_int_opt stride


and pp_name_c_syntax pe f = function
  | CStruct name | CUnion name | ObjcProtocol name ->
      QualifiedCppName.pp f name
  | ObjcClass (name, protocol_names) ->
      F.fprintf f "%a%a" QualifiedCppName.pp name (pp_protocols pe) protocol_names
  | CppClass {name; template_spec_info} ->
      F.fprintf f "%a%a" QualifiedCppName.pp name (pp_template_spec_info pe) template_spec_info
  | JavaClass name ->
      JavaClassName.pp f name
  | CSharpClass name ->
      CSharpClassName.pp f name


and pp_template_spec_info pe f = function
  | NoTemplate ->
      ()
  | Template {args} ->
      let pp_arg_opt f = function
        | TType typ ->
            pp_full pe f typ
        | TInt i ->
            Int64.pp f i
        | TNull ->
            F.pp_print_string f "null"
        | TNullPtr ->
            F.pp_print_string f "NullPtr"
        | TOpaque ->
            F.pp_print_string f "Opaque"
      in
      F.fprintf f "%s%a%s" (escape pe "<") (Pp.comma_seq pp_arg_opt) args (escape pe ">")


and pp_protocols pe f protocols =
  if List.is_empty protocols then ()
  else
    F.fprintf f "%s%a%s" (escape pe "<")
      (Pp.comma_seq (pp_name_c_syntax pe))
      protocols (escape pe ">")


(** Pretty print a type. Do nothing by default. *)
let pp pe f te = if Config.print_types then pp_full pe f te else ()

let to_string typ =
  let pp fmt = pp_full Pp.text fmt typ in
  F.asprintf "%t" pp


let desc_to_string desc =
  let pp fmt = pp_desc Pp.text fmt desc in
  F.asprintf "%t" pp


module Name = struct
  type t = name [@@deriving compare, equal, yojson_of]

  (* NOTE: When a same struct type is used in C/C++/ObjC/ObjC++, their struct types may different,
     eg [CStruct] in, C but [CppClass] in C++.  On the other hand, since [Fieldname.t] includes the
     class names, even for the same field, its field name used in C and C++ can be different.
     However, in analyses, we may want to *not* distinguish fieldnames of the same struct type.  For
     that, we can use these loosened compare functions instead. *)
  let loose_compare x y =
    match (x, y) with
    | ( (CStruct name1 | CppClass {name= name1; template_spec_info= NoTemplate})
      , (CStruct name2 | CppClass {name= name2; template_spec_info= NoTemplate}) ) ->
        QualifiedCppName.compare name1 name2
    | _ ->
        compare x y


  let hash = Hashtbl.hash

  let qual_name = function
    | CStruct name | CUnion name | ObjcProtocol name ->
        name
    | ObjcClass (name, protocol_names) ->
        let protocols = F.asprintf "%a" (pp_protocols Pp.text) protocol_names in
        QualifiedCppName.append_protocols name ~protocols
    | CppClass {name; template_spec_info} ->
        let template_suffix = F.asprintf "%a" (pp_template_spec_info Pp.text) template_spec_info in
        QualifiedCppName.append_template_args_to_last name ~args:template_suffix
    | JavaClass _ | CSharpClass _ ->
        QualifiedCppName.empty


  let unqualified_name = function
    | CStruct name | CUnion name | ObjcProtocol name | ObjcClass (name, _) ->
        name
    | CppClass {name} ->
        name
    | JavaClass _ | CSharpClass _ ->
        QualifiedCppName.empty


  let get_template_spec_info = function
    | CppClass {template_spec_info} ->
        Some template_spec_info
    | _ ->
        None


  let name n =
    match n with
    | CStruct _ | CUnion _ | CppClass _ | ObjcClass _ | ObjcProtocol _ ->
        qual_name n |> QualifiedCppName.to_qual_string
    | JavaClass name ->
        JavaClassName.to_string name
    | CSharpClass name ->
        CSharpClassName.to_string name


  let pp fmt tname =
    let prefix = function
      | CStruct _ ->
          "struct"
      | CUnion _ ->
          "union"
      | CppClass _ | CSharpClass _ | JavaClass _ | ObjcClass _ ->
          "class"
      | ObjcProtocol _ ->
          "protocol"
    in
    F.fprintf fmt "%s %a" (prefix tname) (pp_name_c_syntax Pp.text) tname


  let to_string = F.asprintf "%a" pp

  let is_class = function
    | CppClass _ | JavaClass _ | ObjcClass _ | CSharpClass _ ->
        true
    | _ ->
        false


  let is_union = function CUnion _ -> true | _ -> false

  let is_objc_protocol name = match name with ObjcProtocol _ -> true | _ -> false

  let is_same_type t1 t2 =
    match (t1, t2) with
    | CStruct _, CStruct _
    | CUnion _, CUnion _
    | CppClass _, CppClass _
    | JavaClass _, JavaClass _
    | ObjcClass _, ObjcClass _
    | ObjcProtocol _, ObjcProtocol _
    | CSharpClass _, CSharpClass _ ->
        true
    | _ ->
        false


  module C = struct
    let from_qual_name qual_name = CStruct qual_name

    let from_string name_str = QualifiedCppName.of_qual_string name_str |> from_qual_name

    let union_from_qual_name qual_name = CUnion qual_name
  end

  module CSharp = struct
    let from_string name_str = CSharpClass (CSharpClassName.from_string name_str)
  end

  module Java = struct
    let from_string name_str = JavaClass (JavaClassName.from_string name_str)

    let is_class = function JavaClass _ -> true | _ -> false

    let get_java_class_name_opt typename =
      match typename with JavaClass java_class_name -> Some java_class_name | _ -> None


    let get_java_class_name_exn typename =
      match get_java_class_name_opt typename with
      | Some java_class_name ->
          java_class_name
      | None ->
          L.die InternalError "Tried to split a non-java class name into a java split type@."


    let is_anonymous_inner_class_name_exn class_name =
      let java_class_name = get_java_class_name_exn class_name in
      JavaClassName.is_anonymous_inner_class_name java_class_name


    let is_anonymous_inner_class_name_opt class_name =
      get_java_class_name_opt class_name
      |> Option.map ~f:JavaClassName.is_anonymous_inner_class_name


    let is_external t =
      get_java_class_name_exn t |> JavaClassName.package
      |> Option.exists ~f:Config.java_package_is_external
  end

  module Cpp = struct
    let from_qual_name template_spec_info ~is_union qual_name =
      CppClass {name= qual_name; template_spec_info; is_union}


    let is_class = function CppClass _ -> true | _ -> false
  end

  module Objc = struct
    let from_qual_name qual_name = ObjcClass (qual_name, [])

    let from_string name_str = QualifiedCppName.of_qual_string name_str |> from_qual_name

    let protocol_from_qual_name qual_name = ObjcProtocol qual_name

    let is_class = function ObjcClass _ -> true | _ -> false

    let is_non_tagged_class =
      (* The list of tagged classes are from:
         https://opensource.apple.com/source/objc4/objc4-781/runtime/objc-internal.h *)
      let tagged_classes =
        [ "CGColor"
        ; "NSAtom"
        ; "NSColor"
        ; "NSDate"
        ; "NSIndexPath"
        ; "NSIndexSet"
        ; "NSManagedObjectID"
        ; "NSNumber"
        ; "NSString"
        ; "Photos"
        ; "UIColor" ]
        |> List.map ~f:QualifiedCppName.of_qual_string
        |> QualifiedCppName.Set.of_list
      in
      function
      | ObjcClass (name, _) -> not (QualifiedCppName.Set.mem name tagged_classes) | _ -> false
  end

  module Set = PrettyPrintable.MakePPSet (struct
    type nonrec t = t [@@deriving compare]

    let pp = pp
  end)

  module Map = PrettyPrintable.MakePPMap (struct
    type nonrec t = t [@@deriving compare]

    let pp = pp
  end)

  module Normalizer = HashNormalizer.Make (struct
    type nonrec t = t [@@deriving equal]

    let hash = Hashtbl.hash

    let normalize t =
      match t with
      | CStruct _ | CUnion _ | CppClass _ | ObjcClass _ | ObjcProtocol _ ->
          t
      | JavaClass java_class_name ->
          let java_class_name' = JavaClassName.Normalizer.normalize java_class_name in
          if phys_equal java_class_name java_class_name' then t else JavaClass java_class_name'
      | CSharpClass cs_class_name ->
          let cs_class_name' = CSharpClassName.Normalizer.normalize cs_class_name in
          if phys_equal cs_class_name cs_class_name' then t else CSharpClass cs_class_name'
  end)
end

(** dump a type with all the details. *)
let d_full (t : t) = L.d_pp_with_pe pp_full t

(** dump a list of types. *)
let d_list (tl : t list) =
  let pp pe = Pp.seq (pp pe) in
  L.d_pp_with_pe pp tl


let name typ = match typ.desc with Tstruct name -> Some name | _ -> None

let unsome s = function
  | Some default_typ ->
      default_typ
  | None ->
      L.internal_error "No default typ in %s@." s ;
      assert false


(** turn a *T into a T. fails if [typ] is not a pointer type *)
let strip_ptr typ = match typ.desc with Tptr (t, _) -> t | _ -> assert false

let is_ptr_to_ignore_quals t ~ptr =
  match ptr.desc with Tptr (t', _) -> equal_ignore_quals t t' | _ -> false


(** If an array type, return the type of the element. If not, return the default type if given,
    otherwise raise an exception *)
let array_elem default_opt typ =
  match typ.desc with Tarray {elt} -> elt | _ -> unsome "array_elem" default_opt


let is_class_of_kind check_fun typ =
  match typ.desc with Tstruct tname -> check_fun tname | _ -> false


let is_objc_class = is_class_of_kind Name.Objc.is_class

let is_objc_non_tagged_class = is_class_of_kind Name.Objc.is_non_tagged_class

let is_cpp_class = is_class_of_kind Name.Cpp.is_class

let is_pointer typ = match typ.desc with Tptr _ -> true | _ -> false

let is_reference typ = match typ.desc with Tptr (_, Pk_reference) -> true | _ -> false

let is_struct typ = match typ.desc with Tstruct _ -> true | _ -> false

let is_pointer_to_cpp_class typ = match typ.desc with Tptr (t, _) -> is_cpp_class t | _ -> false

let is_pointer_to_objc_non_tagged_class typ =
  match typ.desc with Tptr (t, _) -> is_objc_non_tagged_class t | _ -> false


let is_pointer_to_void typ = match typ.desc with Tptr ({desc= Tvoid}, _) -> true | _ -> false

let is_void typ = match typ.desc with Tvoid -> true | _ -> false

let is_pointer_to_int typ = match typ.desc with Tptr ({desc= Tint _}, _) -> true | _ -> false

let is_pointer_to_function typ = match typ.desc with Tptr ({desc= Tfun}, _) -> true | _ -> false

let is_int typ = match typ.desc with Tint _ -> true | _ -> false

let is_unsigned_int typ = match typ.desc with Tint ikind -> ikind_is_unsigned ikind | _ -> false

let is_char typ = match typ.desc with Tint ikind -> ikind_is_char ikind | _ -> false

let has_block_prefix s =
  match Str.split_delim (Str.regexp_string Config.anonymous_block_prefix) s with
  | _ :: _ :: _ ->
      true
  | _ ->
      false


let rec pp_java ~verbose f {desc} =
  let string_of_int = function
    | IInt ->
        JConfig.int_st
    | IBool ->
        JConfig.boolean_st
    | ISChar ->
        JConfig.byte_st
    | IUShort ->
        JConfig.char_st
    | ILong ->
        JConfig.long_st
    | IShort ->
        JConfig.short_st
    | _ ->
        L.die InternalError "pp_java int"
  in
  let string_of_float = function
    | FFloat ->
        JConfig.float_st
    | FDouble ->
        JConfig.double_st
    | _ ->
        L.die InternalError "pp_java float"
  in
  match desc with
  | Tint ik ->
      F.pp_print_string f (string_of_int ik)
  | Tfloat fk ->
      F.pp_print_string f (string_of_float fk)
  | Tvoid ->
      F.pp_print_string f JConfig.void
  | Tptr (typ, _) ->
      pp_java ~verbose f typ
  | Tstruct (JavaClass java_class_name) ->
      JavaClassName.pp_with_verbosity ~verbose f java_class_name
  | Tarray {elt} ->
      F.fprintf f "%a[]" (pp_java ~verbose) elt
  | _ ->
      L.die InternalError "pp_java rec"


let rec pp_cs ~verbose f {desc} =
  let string_of_int = function
    | IInt ->
        JConfig.int_st
    | IBool ->
        JConfig.boolean_st
    | ISChar ->
        JConfig.byte_st
    | IUShort ->
        JConfig.char_st
    | ILong ->
        JConfig.long_st
    | IShort ->
        JConfig.short_st
    | _ ->
        L.die InternalError "pp_cs int"
  in
  let string_of_float = function
    | FFloat ->
        JConfig.float_st
    | FDouble ->
        JConfig.double_st
    | _ ->
        L.die InternalError "pp_cs float"
  in
  match desc with
  | Tint ik ->
      F.pp_print_string f (string_of_int ik)
  | Tfloat fk ->
      F.pp_print_string f (string_of_float fk)
  | Tvoid ->
      F.pp_print_string f JConfig.void
  | Tptr (typ, _) ->
      pp_cs ~verbose f typ
  | Tstruct (CSharpClass cs_class_name) ->
      CSharpClassName.pp_with_verbosity ~verbose f cs_class_name
  | Tarray {elt} ->
      F.fprintf f "%a[]" (pp_cs ~verbose) elt
  | _ ->
      L.die InternalError "pp_cs rec"


let is_csharp_primitive_type {desc} =
  let is_csharp_int = function
    | IInt | IBool | ISChar | IUShort | ILong | IShort ->
        true
    | _ ->
        false
  in
  let is_csharp_float = function FFloat | FDouble -> true | _ -> false in
  match desc with Tint ik -> is_csharp_int ik | Tfloat fk -> is_csharp_float fk | _ -> false


let rec is_csharp_type t =
  match t.desc with
  | Tvoid ->
      true
  | Tint _ | Tfloat _ ->
      is_csharp_primitive_type t
  | Tptr ({desc= Tstruct (CSharpClass _)}, Pk_pointer) ->
      true
  | Tptr ({desc= Tarray {elt}}, Pk_pointer) ->
      is_csharp_type elt
  | _ ->
      false


let is_java_primitive_type {desc} =
  let is_java_int = function
    | IInt | IBool | ISChar | IUShort | ILong | IShort ->
        true
    | _ ->
        false
  in
  let is_java_float = function FFloat | FDouble -> true | _ -> false in
  match desc with Tint ik -> is_java_int ik | Tfloat fk -> is_java_float fk | _ -> false


let rec is_java_type t =
  match t.desc with
  | Tvoid ->
      true
  | Tint _ | Tfloat _ ->
      is_java_primitive_type t
  | Tptr ({desc= Tstruct (JavaClass _)}, Pk_pointer) ->
      true
  | Tptr ({desc= Tarray {elt}}, Pk_pointer) ->
      is_java_type elt
  | _ ->
      false


module TypeQualsNormalizer = HashNormalizer.Make (struct
  type t = type_quals [@@deriving equal]

  let hash = Hashtbl.hash

  let normalize = Fn.id
end)

module rec DescNormalizer : (HashNormalizer.S with type t = desc) = HashNormalizer.Make (struct
  type t = desc [@@deriving equal]

  let hash = Hashtbl.hash

  let normalize t =
    match t with
    | Tint _ | Tfloat _ | Tvoid | Tfun ->
        t
    | Tstruct name ->
        let name' = Name.Normalizer.normalize name in
        if phys_equal name name' then t else Tstruct name'
    | TVar str_var ->
        let str_var' = HashNormalizer.StringNormalizer.normalize str_var in
        if phys_equal str_var str_var' then t else TVar str_var'
    | Tptr (pointed, ptr_kind) ->
        let pointed' = Normalizer.normalize pointed in
        if phys_equal pointed pointed' then t else Tptr (pointed', ptr_kind)
    | Tarray {elt; length; stride} ->
        let elt' = Normalizer.normalize elt in
        if phys_equal elt elt' then t else Tarray {elt= elt'; length; stride}
end)

and Normalizer : (HashNormalizer.S with type t = t) = struct
  include HashNormalizer.Make (struct
    include T

    let hash = Hashtbl.hash

    let normalize t =
      let quals = TypeQualsNormalizer.normalize t.quals in
      let desc = DescNormalizer.normalize t.desc in
      if phys_equal desc t.desc && phys_equal quals t.quals then t else {desc; quals}
  end)

  let reset () =
    reset () ;
    TypeQualsNormalizer.reset () ;
    DescNormalizer.reset ()
end
