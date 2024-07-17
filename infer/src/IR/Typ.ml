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
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

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


let ikind_is_unsigned = function
  | IBool | IUChar | IUShort | IUInt | IULong | IULongLong | IU128 ->
      true
  | ISChar | IChar | IShort | IInt | ILong | ILongLong | I128 ->
      false


let ikind_is_char = function IChar | ISChar | IUChar -> true | _ -> false

(** Kinds of floating-point numbers *)
type fkind = FFloat  (** [float] *) | FDouble  (** [double] *) | FLongDouble  (** [long double] *)
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

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
  | Pk_lvalue_reference  (** C++ lvalue reference *)
  | Pk_rvalue_reference  (** C++ rvalue reference *)
  | Pk_objc_weak  (** Obj-C __weak pointer *)
  | Pk_objc_unsafe_unretained  (** Obj-C __unsafe_unretained pointer *)
  | Pk_objc_autoreleasing  (** Obj-C __autoreleasing pointer *)
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

let ptr_kind_string = function
  | Pk_lvalue_reference ->
      "&"
  | Pk_rvalue_reference ->
      "&&"
  | Pk_pointer ->
      "*"
  | Pk_objc_weak ->
      "__weak *"
  | Pk_objc_unsafe_unretained ->
      "__unsafe_unretained *"
  | Pk_objc_autoreleasing ->
      "__autoreleasing *"


(* Note that [is_trivially_copyable] is ignored when compare/equal-ing, since it can be
   inconsistent for the same type depending on compilation units. *)
type type_quals =
  { is_const: bool
  ; is_reference: bool
        (** on the source level - ignoring the additional references Infer's frontend adds *)
  ; is_restrict: bool
  ; is_volatile: bool }
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

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

and template_arg = TType of t | TInt of int64 | TNull | TNullPtr | TOpaque

and template_spec_info = NoTemplate | Template of {mangled: string option; args: template_arg list}
[@@deriving compare, equal, yojson_of, hash, normalize]

let yojson_of_name = [%yojson_of: _]

let is_const {is_const} = is_const

let is_restrict {is_restrict} = is_restrict

let is_volatile {is_volatile} = is_volatile

let escape pe = if Pp.equal_print_kind pe.Pp.kind Pp.HTML then Escape.escape_xml else Fn.id

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
  | ObjcClass name ->
      F.fprintf f "%a" QualifiedCppName.pp name
  | CppClass {name; template_spec_info} ->
      F.fprintf f "%a%a" QualifiedCppName.pp name (pp_template_spec_info pe) template_spec_info
  | ErlangType name ->
      ErlangTypeName.pp f name
  | HackClass name ->
      HackClassName.pp f name
  | JavaClass name ->
      JavaClassName.pp f name
  | CSharpClass name ->
      CSharpClassName.pp f name
  | PythonClass name ->
      PythonClassName.pp f name
  | ObjcBlock bsig ->
      F.fprintf f "%s" bsig.name
  | CFunction csig ->
      F.fprintf f "%a" QualifiedCppName.pp csig.c_name


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


(** Pretty print a type. Do nothing by default. *)
let pp pe f te = if Config.print_types then pp_full pe f te

let make_cpp_class_matcher ?(non_ptr = true) ?(ptr = true) ?prefix fuzzy_qual_names =
  let matcher = QualifiedCppName.Match.of_fuzzy_qual_names ?prefix fuzzy_qual_names in
  function
  | {desc= Tstruct (CppClass {name})} when non_ptr ->
      QualifiedCppName.Match.match_qualifiers matcher name
  | {desc= Tptr ({desc= Tstruct (CppClass {name})}, _)} when ptr ->
      QualifiedCppName.Match.match_qualifiers matcher name
  | _ ->
      false


let is_std_function = make_cpp_class_matcher ~ptr:false ["std::function"]

let is_cpp_lambda = make_cpp_class_matcher ~ptr:false ~prefix:true ["lambda_"]

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


let rec strict_compatible_match formal actual =
  (* Check if formal binds actual, excluding the case where const T& binds const T&&, T&& *)
  match (formal.desc, actual.desc) with
  (* T&& binds T&&
     const T&& binds const T&& and T&& *)
  | Tptr (t1, Pk_rvalue_reference), Tptr (t2, Pk_rvalue_reference) ->
      ((not t2.quals.is_const) || t1.quals.is_const) && strict_compatible_match t1 t2
  (* T& binds T& and T&&
     const T& binds const T&, T&. *)
  | Tptr (t1, Pk_lvalue_reference), Tptr (t2, (Pk_lvalue_reference | Pk_rvalue_reference)) ->
      ((not t2.quals.is_const) || t1.quals.is_const) && strict_compatible_match t1 t2
  (* Any non-reference type T binds T&& *)
  | t1, Tptr (t2, Pk_rvalue_reference) ->
      equal_desc_ignore_quals t1 t2.desc
  (* T&& does not bind any l-value references *)
  | Tptr (_, Pk_rvalue_reference), Tptr (_, Pk_lvalue_reference) ->
      false
  (* Any non-reference type T binds T& *)
  | t1, Tptr (t2, Pk_lvalue_reference) ->
      equal_desc_ignore_quals t1 t2.desc
  | Tptr (t1, ptr_kind1), Tptr (t2, ptr_kind2) ->
      equal_ptr_kind ptr_kind1 ptr_kind2 && strict_compatible_match t1 t2
  | Tint ikind1, Tint ikind2 ->
      equal_ikind ikind1 ikind2
  | Tfloat fkind1, Tfloat fkind2 ->
      equal_fkind fkind1 fkind2
  | Tvoid, Tvoid | Tfun, Tfun ->
      true
  | Tstruct name1, Tstruct name2 ->
      equal_name name1 name2 || (is_std_function formal && is_cpp_lambda actual)
  | TVar s1, TVar s2 ->
      String.equal s1 s2
  | Tarray {elt= t1}, Tarray {elt= t2} ->
      equal_ignore_quals t1 t2
  | _, _ ->
      false


let compatible_match formal actual =
  (* Check if formal binds actual, including the case where const T& binds const T&&, T&&. *)
  match (formal.desc, actual.desc) with
  (* const T& binds const T&&, T&&. *)
  | Tptr (t1, Pk_lvalue_reference), Tptr (t2, Pk_rvalue_reference) ->
      t1.quals.is_const && strict_compatible_match t1 t2
  | _ ->
      strict_compatible_match formal actual


let overloading_resolution = [strict_compatible_match; compatible_match]
(* [overloading_resolution] is a list of predicates that compare whether a type T1 binds a type T2.
    It is ordered by priority, from the highest to the lowest one. The current implementation prioritise
    const T&& / T&& in binding a type const T&&/T&&, as done in the standard (13.3.3.2.3) *)

let mk_type_quals ?default ?is_const ?is_reference ?is_restrict ?is_volatile () =
  let default_ = {is_const= false; is_reference= false; is_restrict= false; is_volatile= false} in
  let mk_aux ?(default = default_) ?(is_const = default.is_const)
      ?(is_reference = default.is_reference) ?(is_restrict = default.is_restrict)
      ?(is_volatile = default.is_volatile) () =
    {is_const; is_reference; is_restrict; is_volatile}
  in
  mk_aux ?default ?is_const ?is_reference ?is_restrict ?is_volatile ()


let is_reference_on_source {is_reference} = is_reference

let is_weak_pointer t =
  match t.desc with Tptr (_, (Pk_objc_weak | Pk_objc_unsafe_unretained)) -> true | _ -> false


let is_strong_pointer t = match t.desc with Tptr (_, Pk_pointer) -> true | _ -> false

let mk ?default ?quals desc : t =
  let default_ = {desc; quals= mk_type_quals ()} in
  let mk_aux ?(default = default_) ?(quals = default.quals) desc = {desc; quals} in
  mk_aux ?default ?quals desc


let set_ptr_to_const ({desc} as typ) =
  match desc with
  | Tptr (t, ptr_kind) ->
      {typ with desc= Tptr ({t with quals= {t.quals with is_const= true}}, ptr_kind)}
  | _ ->
      typ


let set_to_const ({quals} as typ) = {typ with quals= {quals with is_const= true}}

let mk_array ?default ?quals ?length ?stride elt : t =
  mk ?default ?quals (Tarray {elt; length; stride})


let mk_struct name = mk (Tstruct name)

let mk_ptr ?(ptr_kind = Pk_pointer) t = mk (Tptr (t, ptr_kind))

let get_ikind_opt {desc} = match desc with Tint ikind -> Some ikind | _ -> None

(* TODO: size_t should be implementation-dependent. *)
let size_t = IULong

let to_string typ =
  let pp fmt = pp_full Pp.text fmt typ in
  F.asprintf "%t" pp


let desc_to_string desc =
  let pp fmt = pp_desc Pp.text fmt desc in
  F.asprintf "%t" pp


let is_template_spec_info_empty = function
  | NoTemplate | Template {args= []} ->
      true
  | Template {args= _ :: _} ->
      false


module Name = struct
  type t = name [@@deriving compare, equal, yojson_of, sexp, hash, normalize]

  let compare_name x y =
    match (x, y) with
    | ( (CStruct name1 | CUnion name1 | CppClass {name= name1})
      , (CStruct name2 | CUnion name2 | CppClass {name= name2}) ) ->
        QualifiedCppName.compare_name name1 name2
    | (CStruct _ | CUnion _ | CppClass _), _ ->
        -1
    | _, (CStruct _ | CUnion _ | CppClass _) ->
        1
    | CSharpClass name1, CSharpClass name2 ->
        String.compare (CSharpClassName.classname name1) (CSharpClassName.classname name2)
    | CSharpClass _, _ ->
        -1
    | _, CSharpClass _ ->
        1
    | ErlangType name1, ErlangType name2 ->
        ErlangTypeName.compare name1 name2
    | ErlangType _, _ ->
        -1
    | _, ErlangType _ ->
        1
    | JavaClass name1, JavaClass name2 ->
        String.compare (JavaClassName.classname name1) (JavaClassName.classname name2)
    | JavaClass _, _ ->
        -1
    | _, JavaClass _ ->
        1
    | ObjcClass name1, ObjcClass name2 ->
        QualifiedCppName.compare_name name1 name2
    | ObjcClass _, _ ->
        -1
    | _, ObjcClass _ ->
        1
    | ObjcProtocol name1, ObjcProtocol name2 ->
        QualifiedCppName.compare_name name1 name2
    | HackClass name1, HackClass name2 ->
        HackClassName.compare name1 name2
    | HackClass _, _ ->
        -1
    | _, HackClass _ ->
        1
    | PythonClass name1, PythonClass name2 ->
        PythonClassName.compare name1 name2
    | PythonClass _, _ ->
        -1
    | _, PythonClass _ ->
        1
    | ObjcBlock bsig1, ObjcBlock bsig2 ->
        compare_objc_block_sig bsig1 bsig2
    | ObjcBlock _, _ ->
        -1
    | _, ObjcBlock _ ->
        1
    | CFunction csig1, CFunction csig2 ->
        compare_c_function_sig csig1 csig2
    | CFunction _, _ ->
        -1
    | _, CFunction _ ->
        1


  let qual_name = function
    | CStruct name | CUnion name | ObjcProtocol name | ObjcClass name ->
        name
    | CppClass {name; template_spec_info} ->
        let template_suffix = F.asprintf "%a" (pp_template_spec_info Pp.text) template_spec_info in
        QualifiedCppName.append_template_args_to_last name ~args:template_suffix
    | JavaClass _
    | CSharpClass _
    | ErlangType _
    | HackClass _
    | PythonClass _
    | ObjcBlock _
    | CFunction _ ->
        QualifiedCppName.empty


  let unqualified_name = function
    | CStruct name | CUnion name | ObjcProtocol name | ObjcClass name ->
        name
    | CppClass {name} ->
        name
    | JavaClass _
    | CSharpClass _
    | ErlangType _
    | HackClass _
    | PythonClass _
    | ObjcBlock _
    | CFunction _ ->
        QualifiedCppName.empty


  let get_template_spec_info = function
    | CppClass {template_spec_info} ->
        Some template_spec_info
    | _ ->
        None


  let name_without_templates n = unqualified_name n |> QualifiedCppName.to_qual_string

  let name n =
    match n with
    | CStruct _ | CUnion _ | CppClass _ | ObjcClass _ | ObjcProtocol _ ->
        qual_name n |> QualifiedCppName.to_qual_string
    | JavaClass name ->
        JavaClassName.to_string name
    | CSharpClass name ->
        CSharpClassName.to_string name
    | ErlangType name ->
        ErlangTypeName.to_string name
    | HackClass name ->
        HackClassName.to_string name
    | PythonClass name ->
        PythonClassName.to_string name
    | ObjcBlock bsig ->
        bsig.name
    | CFunction csig ->
        QualifiedCppName.to_qual_string csig.c_name


  let pp fmt tname =
    let prefix = function
      | CStruct _ ->
          "struct"
      | CUnion _ ->
          "union"
      | CppClass _ | CSharpClass _ | JavaClass _ | ObjcClass _ ->
          "class"
      | ErlangType _ ->
          "erlang"
      | HackClass _ ->
          "hack"
      | PythonClass _ ->
          "python"
      | ObjcProtocol _ ->
          "protocol"
      | ObjcBlock _ ->
          ""
      | CFunction _ ->
          "function"
    in
    F.fprintf fmt "%s %a" (prefix tname) (pp_name_c_syntax Pp.text) tname


  let to_string = F.asprintf "%a" pp

  let is_class = function
    | CppClass _ | JavaClass _ | HackClass _ | ObjcClass _ | CSharpClass _ | PythonClass _ ->
        true
    | CStruct _ | CUnion _ | ErlangType _ | ObjcProtocol _ | ObjcBlock _ | CFunction _ ->
        false


  let is_union = function CUnion _ -> true | _ -> false

  let is_objc_protocol name = match name with ObjcProtocol _ -> true | _ -> false

  let is_objc_class name = match name with ObjcClass _ -> true | _ -> false

  let is_objc_block name = match name with ObjcBlock _ -> true | _ -> false

  let is_hack_class name = match name with HackClass _ -> true | _ -> false

  let is_python_class name = match name with PythonClass _ -> true | _ -> false

  let is_same_type t1 t2 =
    match (t1, t2) with
    | CStruct _, CStruct _
    | CUnion _, CUnion _
    | CppClass _, CppClass _
    | JavaClass _, JavaClass _
    | HackClass _, HackClass _
    | ObjcClass _, ObjcClass _
    | ObjcProtocol _, ObjcProtocol _
    | CSharpClass _, CSharpClass _ ->
        true
    | _ ->
        false


  module C = struct
    let from_qual_name qual_name =
      if Config.struct_as_cpp_class then
        CppClass {name= qual_name; template_spec_info= NoTemplate; is_union= false}
      else CStruct qual_name


    let from_string name_str = QualifiedCppName.of_qual_string name_str |> from_qual_name

    let union_from_qual_name qual_name = CUnion qual_name
  end

  module CSharp = struct
    let from_string name_str = CSharpClass (CSharpClassName.from_string name_str)

    let is_class = function CSharpClass _ -> true | _ -> false
  end

  module Hack = struct
    let static_companion typename =
      match typename with
      | HackClass class_name ->
          HackClass (HackClassName.static_companion class_name)
      | _ ->
          L.die InternalError "static_companion is only implemented for Hack class names"


    let static_companion_origin typename =
      match typename with
      | HackClass class_name ->
          HackClass (HackClassName.static_companion_origin class_name)
      | _ ->
          L.die InternalError "static_companion_origin is only implemented for Hack class names"


    let is_static_companion typename =
      match typename with
      | HackClass class_name ->
          HackClassName.is_static_companion class_name
      | _ ->
          L.die InternalError "is_static is only implemented for Hack class names"


    let is_generated_curry typename =
      match typename with
      | HackClass class_name ->
          HackClassName.is_generated_curry class_name
      | _ ->
          false


    let extract_curry_info typename =
      match typename with
      | HackClass class_name ->
          HackClassName.extract_curry_info class_name
      | _ ->
          None
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
    let from_qual_name qual_name = ObjcClass qual_name

    let from_string name_str = QualifiedCppName.of_qual_string name_str |> from_qual_name

    let protocol_from_qual_name qual_name = ObjcProtocol qual_name

    let is_class = function ObjcClass _ -> true | _ -> false
  end

  module Set = PrettyPrintable.MakePPSet (struct
    type nonrec t = t [@@deriving compare]

    let pp = pp
  end)

  module Map = PrettyPrintable.MakePPMap (struct
    type nonrec t = t [@@deriving compare]

    let pp = pp
  end)

  module Hash = Hashtbl.Make (struct
    type nonrec t = t [@@deriving equal]

    let hash = hash
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


let is_ptr_to_const typ = match typ.desc with Tptr (t, _) -> is_const t.quals | _ -> false

(** If an array type, return the type of the element. If not, return the default type if given,
    otherwise raise an exception *)
let array_elem default_opt typ =
  match typ.desc with Tarray {elt} -> elt | _ -> unsome "array_elem" default_opt


let is_class_of_kind check_fun typ =
  match typ.desc with Tstruct tname -> check_fun tname | _ -> false


let is_objc_class = is_class_of_kind Name.Objc.is_class

let is_cpp_class = is_class_of_kind Name.Cpp.is_class

let is_pointer typ = match typ.desc with Tptr _ -> true | _ -> false

let is_reference typ =
  match typ.desc with Tptr (_, (Pk_lvalue_reference | Pk_rvalue_reference)) -> true | _ -> false


let is_rvalue_reference typ =
  match typ.desc with Tptr (_, Pk_rvalue_reference) -> true | _ -> false


let is_const_reference_on_source typ =
  is_reference_on_source typ.quals
  && match typ.desc with Tptr ({quals}, Pk_lvalue_reference) -> is_const quals | _ -> false


let is_struct typ = match typ.desc with Tstruct _ -> true | _ -> false

let is_pointer_to_cpp_class typ = match typ.desc with Tptr (t, _) -> is_cpp_class t | _ -> false

let is_pointer_to_const typ = match typ.desc with Tptr ({quals}, _) -> is_const quals | _ -> false

let is_pointer_to_void typ = match typ.desc with Tptr ({desc= Tvoid}, _) -> true | _ -> false

let shared_pointer_qual_names =
  ["std::shared_ptr"; "std::__shared_ptr"; "std::__shared_ptr_access"; "boost::intrusive_ptr"]


let shared_pointer_matcher = QualifiedCppName.Match.of_fuzzy_qual_names shared_pointer_qual_names

let is_pointer_to_smart_pointer =
  make_cpp_class_matcher ~non_ptr:false ("std::unique_ptr" :: shared_pointer_qual_names)


let is_unique_pointer = make_cpp_class_matcher ~ptr:false ["std::unique_ptr"]

let is_pointer_to_unique_pointer = make_cpp_class_matcher ~non_ptr:false ["std::unique_ptr"]

let is_shared_pointer = make_cpp_class_matcher ~ptr:false shared_pointer_qual_names

let is_folly_coro = make_cpp_class_matcher ~prefix:true ["folly::coro"]

let is_void typ = match typ.desc with Tvoid -> true | _ -> false

let is_pointer_to_int typ = match typ.desc with Tptr ({desc= Tint _}, _) -> true | _ -> false

let is_pointer_to_function typ = match typ.desc with Tptr ({desc= Tfun}, _) -> true | _ -> false

let is_int typ = match typ.desc with Tint _ -> true | _ -> false

let is_unsigned_int typ = match typ.desc with Tint ikind -> ikind_is_unsigned ikind | _ -> false

let is_char typ = match typ.desc with Tint ikind -> ikind_is_char ikind | _ -> false

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
