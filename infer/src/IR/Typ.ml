(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
  [@@deriving compare]

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


let ikind_is_char = function IChar | ISChar | IUChar -> true | _ -> false

let ikind_is_unsigned = function
  | IUChar | IUInt | IUShort | IULong | IULongLong ->
      true
  | _ ->
      false


(** Kinds of floating-point numbers *)
type fkind =
  | FFloat  (** [float] *)
  | FDouble  (** [double] *)
  | FLongDouble  (** [long double] *)
  [@@deriving compare]

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
  [@@deriving compare]

let equal_ptr_kind = [%compare.equal : ptr_kind]

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
  type type_quals = {is_const: bool; is_restrict: bool; is_volatile: bool} [@@deriving compare]

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
    | CppClass of QualifiedCppName.t * template_spec_info
    | JavaClass of Mangled.t
    | ObjcClass of QualifiedCppName.t
    | ObjcProtocol of QualifiedCppName.t
    [@@deriving compare]

  and template_arg =
    | TType of t
    | TInt of Int64.t
    | TNull
    | TNullPtr
    | TOpaque
    [@@deriving compare]

  and template_spec_info =
    | NoTemplate
    | Template of {mangled: string option; args: template_arg list}
    [@@deriving compare]

  let equal_desc = [%compare.equal : desc]

  let equal_quals = [%compare.equal : type_quals]

  let equal = [%compare.equal : t]

  let hash = Hashtbl.hash
end

include T

let mk_type_quals ?default ?is_const ?is_restrict ?is_volatile () =
  let default_ = {is_const= false; is_restrict= false; is_volatile= false} in
  let mk_aux ?(default= default_) ?(is_const= default.is_const) ?(is_restrict= default.is_restrict)
      ?(is_volatile= default.is_volatile) () =
    {is_const; is_restrict; is_volatile}
  in
  mk_aux ?default ?is_const ?is_restrict ?is_volatile ()


let is_const {is_const} = is_const

let is_restrict {is_restrict} = is_restrict

let is_volatile {is_volatile} = is_volatile

let mk ?default ?quals desc : t =
  let default_ = {desc; quals= mk_type_quals ()} in
  let mk_aux ?(default= default_) ?(quals= default.quals) desc = {desc; quals} in
  mk_aux ?default ?quals desc


let mk_array ?default ?quals ?length ?stride elt : t =
  mk ?default ?quals (Tarray {elt; length; stride})


let void_star = mk (Tptr (mk Tvoid, Pk_pointer))

let merge_quals quals1 quals2 =
  { is_const= quals1.is_const || quals2.is_const
  ; is_restrict= quals1.is_restrict || quals2.is_restrict
  ; is_volatile= quals1.is_volatile || quals2.is_volatile }


let escape pe = if Pp.equal_print_kind pe.Pp.kind Pp.HTML then Escape.escape_xml else ident

(** Pretty print a type with all the details, using the C syntax. *)
let rec pp_full pe f typ =
  let pp_quals f {quals} =
    if is_const quals then F.fprintf f " const " ;
    if is_restrict quals then F.fprintf f " __restrict " ;
    if is_volatile quals then F.fprintf f " volatile "
  in
  let pp_desc f {desc} =
    match desc with
    | Tstruct tname ->
        F.fprintf f "%a" (pp_name_c_syntax pe) tname
    | TVar name ->
        F.fprintf f "%s" name
    | Tint ik ->
        F.fprintf f "%s" (ikind_to_string ik)
    | Tfloat fk ->
        F.fprintf f "%s" (fkind_to_string fk)
    | Tvoid ->
        F.fprintf f "void"
    | Tfun {no_return= false} ->
        F.fprintf f "_fn_"
    | Tfun {no_return= true} ->
        F.fprintf f "_fn_noreturn_"
    | Tptr (({desc= Tarray _ | Tfun _} as typ), pk) ->
        F.fprintf f "%a(%s)" (pp_full pe) typ (ptr_kind_string pk |> escape pe)
    | Tptr (typ, pk) ->
        F.fprintf f "%a%s" (pp_full pe) typ (ptr_kind_string pk |> escape pe)
    | Tarray {elt; length; stride} ->
        let pp_int_opt fmt = function Some x -> IntLit.pp fmt x | None -> F.fprintf fmt "_" in
        F.fprintf f "%a[%a*%a]" (pp_full pe) elt pp_int_opt length pp_int_opt stride
  in
  F.fprintf f "%a%a" pp_desc typ pp_quals typ


and pp_name_c_syntax pe f = function
  | CStruct name | CUnion name | ObjcClass name | ObjcProtocol name ->
      F.fprintf f "%a" QualifiedCppName.pp name
  | CppClass (name, template_spec) ->
      F.fprintf f "%a%a" QualifiedCppName.pp name (pp_template_spec_info pe) template_spec
  | JavaClass name ->
      F.fprintf f "%a" Mangled.pp name


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
            Pp.string f "null"
        | TNullPtr ->
            Pp.string f "NullPtr"
        | TOpaque ->
            Pp.string f "Opaque"
      in
      F.fprintf f "%s%a%s" (escape pe "<") (Pp.comma_seq pp_arg_opt) args (escape pe ">")


(** Pretty print a type. Do nothing by default. *)
let pp pe f te = if Config.print_types then pp_full pe f te else ()

let to_string typ =
  let pp fmt = pp_full Pp.text fmt typ in
  F.asprintf "%t" pp


type type_subst_t = (string * t) list [@@deriving compare]

let is_type_subst_empty = List.is_empty

(** Given the template type mapping and the type, substitute tvars within the type. *)
let rec sub_type subst generic_typ : t =
  match generic_typ.desc with
  | TVar tname -> (
    match List.Assoc.find subst ~equal:String.equal tname with
    | Some t ->
        (* Type qualifiers may come from original type or be part of substitution. Merge them *)
        mk ~quals:(merge_quals t.quals generic_typ.quals) t.desc
    | None ->
        generic_typ )
  | Tarray {elt= typ; length; stride} ->
      let typ' = sub_type subst typ in
      if phys_equal typ typ' then generic_typ
      else mk_array ~default:generic_typ typ' ?length ?stride
  | Tptr (typ, arg) ->
      let typ' = sub_type subst typ in
      if phys_equal typ typ' then generic_typ else mk ~default:generic_typ (Tptr (typ', arg))
  | Tstruct tname ->
      let tname' = sub_tname subst tname in
      if phys_equal tname tname' then generic_typ else mk ~default:generic_typ (Tstruct tname')
  | _ ->
      generic_typ


and sub_tname subst tname =
  match tname with
  | CppClass (name, Template {mangled; args}) ->
      let sub_typ_opt typ_opt =
        match typ_opt with
        | TType typ ->
            let typ' = sub_type subst typ in
            if phys_equal typ typ' then typ_opt else TType typ'
        | TInt _ | TNull | TNullPtr | TOpaque ->
            typ_opt
      in
      let args' = IList.map_changed sub_typ_opt args in
      if phys_equal args args' then tname else CppClass (name, Template {mangled; args= args'})
  | _ ->
      tname


module Name = struct
  type t = name [@@deriving compare]

  let equal = [%compare.equal : t]

  let qual_name = function
    | CStruct name | CUnion name | ObjcClass name | ObjcProtocol name ->
        name
    | CppClass (name, templ_args) ->
        let template_suffix = F.asprintf "%a" (pp_template_spec_info Pp.text) templ_args in
        QualifiedCppName.append_template_args_to_last name ~args:template_suffix
    | JavaClass _ ->
        QualifiedCppName.empty


  let unqualified_name = function
    | CStruct name | CUnion name | ObjcClass name | ObjcProtocol name ->
        name
    | CppClass (name, _) ->
        name
    | JavaClass _ ->
        QualifiedCppName.empty


  let name n =
    match n with
    | CStruct _ | CUnion _ | CppClass _ | ObjcClass _ | ObjcProtocol _ ->
        qual_name n |> QualifiedCppName.to_qual_string
    | JavaClass name ->
        Mangled.to_string name


  let pp fmt tname =
    let prefix = function
      | CStruct _ ->
          "struct"
      | CUnion _ ->
          "union"
      | CppClass _ | JavaClass _ | ObjcClass _ ->
          "class"
      | ObjcProtocol _ ->
          "protocol"
    in
    F.fprintf fmt "%s %a" (prefix tname) (pp_name_c_syntax Pp.text) tname


  let to_string = F.asprintf "%a" pp

  let is_class = function CppClass _ | JavaClass _ | ObjcClass _ -> true | _ -> false

  let is_same_type t1 t2 =
    match (t1, t2) with
    | CStruct _, CStruct _
    | CUnion _, CUnion _
    | CppClass _, CppClass _
    | JavaClass _, JavaClass _
    | ObjcClass _, ObjcClass _
    | ObjcProtocol _, ObjcProtocol _ ->
        true
    | _ ->
        false


  module C = struct
    let from_qual_name qual_name = CStruct qual_name

    let from_string name_str = QualifiedCppName.of_qual_string name_str |> from_qual_name

    let union_from_qual_name qual_name = CUnion qual_name
  end

  module Java = struct
    let from_string name_str = JavaClass (Mangled.from_string name_str)

    let from_package_class package_name class_name =
      if String.equal package_name "" then from_string class_name
      else from_string (package_name ^ "." ^ class_name)


    let is_class = function JavaClass _ -> true | _ -> false

    let java_lang_object = from_string "java.lang.Object"

    let java_io_serializable = from_string "java.io.Serializable"

    let java_lang_cloneable = from_string "java.lang.Cloneable"

    (** Given a package.class_name string, it looks for the latest dot and split the string
        in two (package, class_name) *)
    let split_classname package_classname =
      match String.rsplit2 package_classname ~on:'.' with
      | Some (x, y) ->
          (Some x, y)
      | None ->
          (None, package_classname)


    let split_typename typename = split_classname (name typename)

    let get_outer_class class_name =
      let package_name, class_name_no_package = split_typename class_name in
      match String.rsplit2 ~on:'$' class_name_no_package with
      | Some (parent_class, _) ->
          Some (from_package_class (Option.value ~default:"" package_name) parent_class)
      | None ->
          None


    let is_anonymous_inner_class_name class_name =
      let class_name_no_package = snd (split_typename class_name) in
      match String.rsplit2 class_name_no_package ~on:'$' with
      | Some (_, s) ->
          let is_int =
            try
              ignore (int_of_string (String.strip s)) ;
              true
            with Failure _ -> false
          in
          is_int
      | None ->
          false


    let is_external_classname name_string =
      let package, _ = split_classname name_string in
      Option.exists ~f:Config.java_package_is_external package


    let is_external t = is_external_classname (name t)
  end

  module Cpp = struct
    let from_qual_name template_spec_info qual_name = CppClass (qual_name, template_spec_info)

    let is_class = function CppClass _ -> true | _ -> false
  end

  module Objc = struct
    let from_qual_name qual_name = ObjcClass qual_name

    let from_string name_str = QualifiedCppName.of_qual_string name_str |> from_qual_name

    let protocol_from_qual_name qual_name = ObjcProtocol qual_name

    let is_class = function ObjcClass _ -> true | _ -> false
  end

  module Set = Caml.Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end

(** {2 Sets and maps of types} *)
module Set = Caml.Set.Make (T)
module Map = Caml.Map.Make (T)
module Tbl = Hashtbl.Make (T)

(** dump a type with all the details. *)
let d_full (t: t) = L.add_print_action (L.PTtyp_full, Obj.repr t)

(** dump a list of types. *)
let d_list (tl: t list) = L.add_print_action (L.PTtyp_list, Obj.repr tl)

let name typ = match typ.desc with Tstruct name -> Some name | _ -> None

let inner_name typ = match typ.desc with Tptr ({desc= Tstruct name}, _) -> Some name | _ -> None

let unsome s = function
  | Some default_typ ->
      default_typ
  | None ->
      L.internal_error "No default typ in %s@." s ;
      assert false


(** turn a *T into a T. fails if [typ] is not a pointer type *)
let strip_ptr typ = match typ.desc with Tptr (t, _) -> t | _ -> assert false

(** If an array type, return the type of the element.
    If not, return the default type if given, otherwise raise an exception *)
let array_elem default_opt typ =
  match typ.desc with Tarray {elt} -> elt | _ -> unsome "array_elem" default_opt


let is_class_of_kind check_fun typ =
  match typ.desc with Tstruct tname -> check_fun tname | _ -> false


let is_objc_class = is_class_of_kind Name.Objc.is_class

let is_cpp_class = is_class_of_kind Name.Cpp.is_class

let is_pointer typ = match typ.desc with Tptr _ -> true | _ -> false

let is_pointer_to_cpp_class typ = match typ.desc with Tptr (t, _) -> is_cpp_class t | _ -> false

let has_block_prefix s =
  match Str.split_delim (Str.regexp_string Config.anonymous_block_prefix) s with
  | _ :: _ :: _ ->
      true
  | _ ->
      false


type typ = t

module Procname = struct
  (** Level of verbosity of some to_string functions. *)
  type detail_level = Verbose | Non_verbose | Simple [@@deriving compare]

  let equal_detail_level = [%compare.equal : detail_level]

  let is_verbose v = match v with Verbose -> true | _ -> false

  module Java = struct
    type kind =
      | Non_Static
      (* in Java, procedures called with invokevirtual, invokespecial, and invokeinterface *)
      | Static
      (* in Java, procedures called with invokestatic *)
      [@@deriving compare]

    (* TODO: use Mangled.t here *)
    type java_type = string option * string

    (* compare in inverse order *)
    let compare_java_type (p1, c1) (p2, c2) = [%compare : string * string option] (c1, p1) (c2, p2)

    (** Type of java procedure names. *)
    type t =
      { method_name: string
      ; parameters: java_type list
      ; class_name: Name.t
      ; return_type: java_type option (* option because constructors have no return type *)
      ; kind: kind }
      [@@deriving compare]

    let make class_name return_type method_name parameters kind =
      {class_name; return_type; method_name; parameters; kind}


    (** A type is a pair (package, type_name) that is translated in a string package.type_name *)
    let type_to_string_verbosity p verbosity =
      match p with
      | None, typ ->
          typ
      | Some p, cls ->
          if is_verbose verbosity then p ^ "." ^ cls else cls


    (** Given a list of types, it creates a unique string of types separated by commas *)
    let rec param_list_to_string inputList verbosity =
      match inputList with
      | [] ->
          ""
      | [head] ->
          type_to_string_verbosity head verbosity
      | head :: rest ->
          type_to_string_verbosity head verbosity ^ "," ^ param_list_to_string rest verbosity


    (** It is the same as java_type_to_string_verbosity, but Java return types are optional because
        of constructors without type *)
    let return_type_to_string j verbosity =
      match j.return_type with None -> "" | Some typ -> type_to_string_verbosity typ verbosity


    let get_class_name j = Name.name j.class_name

    let get_class_type_name j = j.class_name

    let get_simple_class_name j = snd (Name.Java.split_classname (get_class_name j))

    let get_package j = fst (Name.Java.split_classname (get_class_name j))

    let get_method j = j.method_name

    let replace_method j mname = {j with method_name= mname}

    let replace_return_type j ret_type = {j with return_type= Some ret_type}

    let replace_parameters j parameters = {j with parameters}

    let get_return_type j = return_type_to_string j Verbose

    let get_parameters j = j.parameters

    (** Prints a string of a java procname with the given level of verbosity *)
    let to_string ?(withclass= false) j verbosity =
      match verbosity with
      | Verbose | Non_verbose ->
          (* if verbose, then package.class.method(params): rtype,
         else rtype package.class.method(params)
         verbose is used for example to create unique filenames, non_verbose to create reports *)
          let return_type = return_type_to_string j verbosity in
          let params = param_list_to_string j.parameters verbosity in
          let class_name =
            type_to_string_verbosity (Name.Java.split_typename j.class_name) verbosity
          in
          let separator =
            match (j.return_type, verbosity) with
            | None, _ ->
                ""
            | Some _, Verbose ->
                ":"
            | _ ->
                " "
          in
          let output = class_name ^ "." ^ j.method_name ^ "(" ^ params ^ ")" in
          if equal_detail_level verbosity Verbose then output ^ separator ^ return_type
          else return_type ^ separator ^ output
      | Simple ->
          (* methodname(...) or without ... if there are no parameters *)
          let cls_prefix =
            if withclass then
              type_to_string_verbosity (Name.Java.split_typename j.class_name) verbosity ^ "."
            else ""
          in
          let params = match j.parameters with [] -> "" | _ -> "..." in
          let method_name =
            if String.equal j.method_name "<init>" then get_simple_class_name j
            else cls_prefix ^ j.method_name
          in
          method_name ^ "(" ^ params ^ ")"


    let get_return_typ pname_java =
      let rec java_from_string = function
        | "" | "void" ->
            mk Tvoid
        | "int" ->
            mk (Tint IInt)
        | "byte" ->
            mk (Tint IShort)
        | "short" ->
            mk (Tint IShort)
        | "boolean" ->
            mk (Tint IBool)
        | "char" ->
            mk (Tint IChar)
        | "long" ->
            mk (Tint ILong)
        | "float" ->
            mk (Tfloat FFloat)
        | "double" ->
            mk (Tfloat FDouble)
        | typ_str when String.contains typ_str '[' ->
            let stripped_typ = String.sub typ_str ~pos:0 ~len:(String.length typ_str - 2) in
            mk (Tptr (mk_array (java_from_string stripped_typ), Pk_pointer))
        | typ_str ->
            mk (Tstruct (Name.Java.from_string typ_str))
      in
      let typ = java_from_string (get_return_type pname_java) in
      match typ.desc with Tstruct _ -> mk (Tptr (typ, Pk_pointer)) | _ -> typ


    let is_close {method_name} = String.equal method_name "close"

    let is_class_initializer {method_name} = String.equal method_name "<clinit>"

    let is_anonymous_inner_class_constructor {class_name} =
      Name.Java.is_anonymous_inner_class_name class_name


    let is_static {kind} = match kind with Static -> true | _ -> false

    let is_lambda {method_name} = String.is_prefix ~prefix:"lambda$" method_name

    let is_generated {method_name} = String.is_prefix ~prefix:"$" method_name

    let is_access_method {method_name} =
      match String.rsplit2 method_name ~on:'$' with
      | Some ("access", s) ->
          let is_int =
            try
              ignore (int_of_string s) ;
              true
            with Failure _ -> false
          in
          is_int
      | _ ->
          false


    let is_autogen_method {method_name} = String.contains method_name '$'

    (** Check if the proc name has the type of a java vararg.
      Note: currently only checks that the last argument has type Object[]. *)
    let is_vararg {parameters} =
      match List.last parameters with Some (_, "java.lang.Object[]") -> true | _ -> false
  end

  module ObjC_Cpp = struct
    type kind =
      | CPPMethod of {mangled: string option}
      | CPPConstructor of {mangled: string option; is_constexpr: bool}
      | CPPDestructor of {mangled: string option}
      | ObjCClassMethod
      | ObjCInstanceMethod
      | ObjCInternalMethod
      [@@deriving compare]

    type t =
      { method_name: string
      ; class_name: Name.t
      ; kind: kind
      ; template_args: template_spec_info
      ; is_generic_model: bool }
      [@@deriving compare]

    let make class_name method_name kind template_args ~is_generic_model =
      {class_name; method_name; kind; template_args; is_generic_model}


    let get_class_name objc_cpp = Name.name objc_cpp.class_name

    let get_class_type_name objc_cpp = objc_cpp.class_name

    let get_class_qualifiers objc_cpp = Name.qual_name objc_cpp.class_name

    let objc_method_kind_of_bool is_instance =
      if is_instance then ObjCInstanceMethod else ObjCClassMethod


    let is_objc_constructor method_name =
      String.equal method_name "new" || String.is_prefix ~prefix:"init" method_name


    let is_objc_kind = function
      | ObjCClassMethod | ObjCInstanceMethod | ObjCInternalMethod ->
          true
      | _ ->
          false


    let is_objc_method {kind} = is_objc_kind kind

    let is_objc_dealloc method_name = String.equal method_name "dealloc"

    let is_destructor = function
      | {kind= CPPDestructor _} ->
          true
      | name ->
          is_objc_dealloc name.method_name


    let is_constexpr = function {kind= CPPConstructor {is_constexpr= true}} -> true | _ -> false

    let is_cpp_lambda {method_name} = String.is_substring ~substring:"operator()" method_name

    let is_operator_equal {method_name} = String.is_substring ~substring:"operator=" method_name

    let kind_to_verbose_string = function
      | CPPMethod {mangled} | CPPDestructor {mangled} ->
          "(" ^ Option.value ~default:"" mangled ^ ")"
      | CPPConstructor {mangled; is_constexpr} ->
          "{" ^ Option.value ~default:"" mangled ^ (if is_constexpr then "|constexpr" else "")
          ^ "}"
      | ObjCClassMethod ->
          "class"
      | ObjCInstanceMethod ->
          "instance"
      | ObjCInternalMethod ->
          "internal"


    let to_string osig detail_level =
      match detail_level with
      | Simple ->
          osig.method_name
      | Non_verbose ->
          Name.name osig.class_name ^ "_" ^ osig.method_name
      | Verbose ->
          let m_str = kind_to_verbose_string osig.kind in
          Name.name osig.class_name ^ "_" ^ osig.method_name ^ m_str
  end

  (** Type of c procedure names. *)
  type c =
    { name: QualifiedCppName.t
    ; mangled: string option
    ; template_args: template_spec_info
    ; is_generic_model: bool }
    [@@deriving compare]

  (** Type of Objective C block names. *)
  type block_name = string [@@deriving compare]

  (** Type of procedure names. *)
  type t =
    | Java of Java.t
    | C of c
    | Linters_dummy_method
    | Block of block_name
    | ObjC_Cpp of ObjC_Cpp.t
    | WithBlockParameters of t * block_name list
    [@@deriving compare]

  let equal = [%compare.equal : t]

  let hash = Hashtbl.hash

  let block_name_of_procname procname =
    match procname with
    | Block block_name ->
        block_name
    | _ ->
        Logging.die InternalError "Only to be called with Objective-C block names"


  let empty_block = Block ""

  let c name mangled template_args ~is_generic_model =
    {name; mangled= Some mangled; template_args; is_generic_model}


  let from_string_c_fun (name: string) =
    C
      { name= QualifiedCppName.of_qual_string name
      ; mangled= None
      ; template_args= NoTemplate
      ; is_generic_model= false }


  let with_block_parameters base blocks = WithBlockParameters (base, blocks)

  (** Create an objc procedure name from a class_name and method_name. *)
  let mangled_objc_block name = Block name

  let is_java = function Java _ -> true | _ -> false

  (* TODO: deprecate this unfortunately named function and use is_clang instead *)
  let is_c_method = function ObjC_Cpp _ -> true | _ -> false

  let is_c_function = function C _ -> true | _ -> false

  let is_clang = function
    | ObjC_Cpp name ->
        ObjC_Cpp.is_objc_method name
    | name ->
        is_c_function name


  (** Replace the class name component of a procedure name.
      In case of Java, replace package and class name. *)
  let rec replace_class t (new_class: Name.t) =
    match t with
    | Java j ->
        Java {j with class_name= new_class}
    | ObjC_Cpp osig ->
        ObjC_Cpp {osig with class_name= new_class}
    | WithBlockParameters (base, blocks) ->
        WithBlockParameters (replace_class base new_class, blocks)
    | C _ | Block _ | Linters_dummy_method ->
        t


  let rec objc_cpp_replace_method_name t (new_method_name: string) =
    match t with
    | ObjC_Cpp osig ->
        ObjC_Cpp {osig with method_name= new_method_name}
    | WithBlockParameters (base, blocks) ->
        WithBlockParameters (objc_cpp_replace_method_name base new_method_name, blocks)
    | C _ | Block _ | Linters_dummy_method | Java _ ->
        t


  (** Return the method/function of a procname. *)
  let rec get_method = function
    | ObjC_Cpp name ->
        name.method_name
    | WithBlockParameters (base, _) ->
        get_method base
    | C {name} ->
        QualifiedCppName.to_qual_string name
    | Block name ->
        name
    | Java j ->
        j.method_name
    | Linters_dummy_method ->
        "Linters_dummy_method"


  (** Return whether the procname is a block procname. *)
  let is_objc_block = function Block _ -> true | _ -> false

  (** Return the language of the procedure. *)
  let get_language = function
    | ObjC_Cpp _ ->
        Language.Clang
    | C _ ->
        Language.Clang
    | Block _ ->
        Language.Clang
    | Linters_dummy_method ->
        Language.Clang
    | WithBlockParameters _ ->
        Language.Clang
    | Java _ ->
        Language.Java


  (** [is_constructor pname] returns true if [pname] is a constructor *)
  let is_constructor = function
    | Java js ->
        String.equal js.method_name "<init>"
    | ObjC_Cpp {kind= CPPConstructor _} ->
        true
    | ObjC_Cpp {kind; method_name} when ObjC_Cpp.is_objc_kind kind ->
        ObjC_Cpp.is_objc_constructor method_name
    | _ ->
        false


  (** [is_infer_undefined pn] returns true if [pn] is a special Infer undefined proc *)
  let is_infer_undefined pn =
    match pn with
    | Java j ->
        let regexp = Str.regexp "com.facebook.infer.builtins.InferUndefined" in
        Str.string_match regexp (Java.get_class_name j) 0
    | _ ->
        (* TODO: add cases for obj-c, c, c++ *)
        false


  let get_global_name_of_initializer = function
    | C {name}
      when String.is_prefix ~prefix:Config.clang_initializer_prefix
             (QualifiedCppName.to_qual_string name) ->
        let name_str = QualifiedCppName.to_qual_string name in
        let prefix_len = String.length Config.clang_initializer_prefix in
        Some (String.sub name_str ~pos:prefix_len ~len:(String.length name_str - prefix_len))
    | _ ->
        None


  (** to_string for C_function type *)
  let to_readable_string (c1, c2) verbose =
    let plain = QualifiedCppName.to_qual_string c1 in
    if verbose then match c2 with None -> plain | Some s -> plain ^ "{" ^ s ^ "}" else plain


  let with_blocks_parameters_to_string base blocks to_string_f =
    let base_id = to_string_f base in
    String.concat ~sep:"_" (base_id :: blocks)


  (** Very verbose representation of an existing Procname.t *)
  let rec to_unique_id pn =
    match pn with
    | Java j ->
        Java.to_string j Verbose
    | C {name; mangled} ->
        to_readable_string (name, mangled) true
    | ObjC_Cpp osig ->
        ObjC_Cpp.to_string osig Verbose
    | Block name ->
        name
    | WithBlockParameters (base, blocks) ->
        with_blocks_parameters_to_string base blocks to_unique_id
    | Linters_dummy_method ->
        "Linters_dummy_method"


  (** Convert a proc name to a string for the user to see *)
  let rec to_string p =
    match p with
    | Java j ->
        Java.to_string j Non_verbose
    | C {name; mangled} ->
        to_readable_string (name, mangled) false
    | ObjC_Cpp osig ->
        ObjC_Cpp.to_string osig Non_verbose
    | Block name ->
        name
    | WithBlockParameters (base, blocks) ->
        with_blocks_parameters_to_string base blocks to_string
    | Linters_dummy_method ->
        to_unique_id p


  (** Convenient representation of a procname for external tools (e.g. eclipse plugin) *)
  let rec to_simplified_string ?(withclass= false) p =
    match p with
    | Java j ->
        Java.to_string ~withclass j Simple
    | C {name; mangled} ->
        to_readable_string (name, mangled) false ^ "()"
    | ObjC_Cpp osig ->
        ObjC_Cpp.to_string osig Simple
    | Block _ ->
        "block"
    | WithBlockParameters (base, _) ->
        to_simplified_string base
    | Linters_dummy_method ->
        to_unique_id p


  let hashable_name p =
    match p with
    | Java pname ->
        (* Strip autogenerated anonymous inner class numbers in order to keep the bug hash
           invariant when introducing new annonynous classes *)
        Str.global_replace (Str.regexp "$[0-9]+") "$_"
          (Java.to_string ~withclass:true pname Simple)
    | ObjC_Cpp m when ObjC_Cpp.is_objc_method m ->
        (* In Objective C, the list of parameters is part of the method name. To prevent the bug
           hash to change when a parameter is introduced or removed, only the part of the name
           before the first colon is used for the bug hash *)
        List.hd_exn (String.split_on_chars (to_simplified_string ~withclass:true p) ~on:[':'])
    | _ ->
        (* Other cases for C and C++ method names *)
        to_simplified_string ~withclass:true p


  (** Pretty print a proc name *)
  let pp f pn = F.fprintf f "%s" (to_string pn)

  (** hash function for procname *)
  let hash_pname = Hashtbl.hash

  module Hashable = struct
    type nonrec t = t

    let equal = equal

    let hash = hash_pname
  end

  module Hash = Hashtbl.Make (Hashable)

  module Map = PrettyPrintable.MakePPMap (struct
    type nonrec t = t

    let compare = compare

    let pp = pp
  end)

  module Set = PrettyPrintable.MakePPSet (struct
    type nonrec t = t

    let compare = compare

    let pp = pp
  end)

  let get_qualifiers pname =
    match pname with
    | C {name} ->
        name
    | ObjC_Cpp objc_cpp ->
        ObjC_Cpp.get_class_qualifiers objc_cpp
        |> QualifiedCppName.append_qualifier ~qual:objc_cpp.method_name
    | _ ->
        QualifiedCppName.empty


  (** Convert a proc name to a filename *)
  let to_concrete_filename ?crc_only pname =
    (* filenames for clang procs are REVERSED qualifiers with '#' as separator *)
    let get_qual_name_str pname =
      get_qualifiers pname |> QualifiedCppName.to_rev_list |> String.concat ~sep:"#"
    in
    let proc_id =
      match pname with
      | C {mangled} ->
          get_qual_name_str pname :: Option.to_list mangled |> String.concat ~sep:"#"
      | ObjC_Cpp objc_cpp ->
          get_qual_name_str pname ^ "#" ^ ObjC_Cpp.kind_to_verbose_string objc_cpp.kind
      | _ ->
          to_unique_id pname
    in
    Escape.escape_filename @@ DB.append_crc_cutoff ?crc_only proc_id


  let to_generic_filename ?crc_only pname =
    let proc_id =
      get_qualifiers pname |> QualifiedCppName.strip_template_args |> QualifiedCppName.to_rev_list
      |> String.concat ~sep:"#"
    in
    Escape.escape_filename @@ DB.append_crc_cutoff ?crc_only proc_id


  let to_filename ?crc_only pname =
    match pname with
    | (C {is_generic_model} | ObjC_Cpp {is_generic_model}) when Bool.equal is_generic_model true ->
        to_generic_filename ?crc_only pname
    | _ ->
        to_concrete_filename ?crc_only pname


  module SQLite = struct
    let pname_to_key =
      Base.Hashtbl.create
        ( module struct
          type nonrec t = t

          let compare = compare

          let hash = hash

          let sexp_of_t p = Sexp.Atom (to_string p)
        end )
        ()


    let serialize pname =
      let default () = Sqlite3.Data.TEXT (to_filename pname) in
      Base.Hashtbl.find_or_add pname_to_key pname ~default


    let clear_cache () = Base.Hashtbl.clear pname_to_key
  end

  module SQLiteList = SqliteUtils.MarshalledData (struct
    type nonrec t = t list
  end)
end

module Fieldname = struct
  type t =
    | Clang of {class_name: Name.t; field_name: string}
    | Java of string
    [@@deriving compare]

  let equal = [%compare.equal : t]

  module T = struct
    type nonrec t = t

    let compare = compare
  end

  module Set = Caml.Set.Make (T)
  module Map = Caml.Map.Make (T)

  (** Convert a fieldname to a string. *)
  let to_string = function Java fname -> fname | Clang {field_name} -> field_name

  (** Convert a fieldname to a simplified string with at most one-level path. *)
  let to_simplified_string fn =
    let s = to_string fn in
    match String.rsplit2 s ~on:'.' with
    | Some (s1, s2) -> (
      match String.rsplit2 s1 ~on:'.' with Some (_, s4) -> s4 ^ "." ^ s2 | _ -> s )
    | _ ->
        s


  let to_full_string fname =
    match fname with
    | Clang {class_name; field_name} ->
        Name.to_string class_name ^ "::" ^ field_name
    | _ ->
        to_string fname


  (** Convert a fieldname to a flat string without path. *)
  let to_flat_string fn =
    let s = to_string fn in
    match String.rsplit2 s ~on:'.' with Some (_, s2) -> s2 | _ -> s


  let pp f = function Java field_name | Clang {field_name} -> Format.fprintf f "%s" field_name

  let class_name_replace fname ~f =
    match fname with
    | Clang {class_name; field_name} ->
        let class_name' = f class_name in
        if phys_equal class_name class_name' then fname
        else Clang {class_name= class_name'; field_name}
    | _ ->
        fname


  let clang_get_qual_class = function
    | Clang {class_name} ->
        Some (Name.qual_name class_name)
    | _ ->
        None


  module Clang = struct
    let from_class_name class_name field_name = Clang {class_name; field_name}
  end

  module Java = struct
    let from_string n = Java n

    let is_captured_parameter field_name =
      match field_name with
      | Java _ ->
          String.is_prefix ~prefix:"val$" (to_flat_string field_name)
      | Clang _ ->
          false


    let get_class fn =
      let fn = to_string fn in
      let ri = String.rindex_exn fn '.' in
      String.slice fn 0 ri


    let get_field fn =
      let fn = to_string fn in
      let ri = 1 + String.rindex_exn fn '.' in
      String.slice fn ri 0


    let is_outer_instance fn =
      let fn = to_string fn in
      let fn_len = String.length fn in
      fn_len <> 0
      &&
      let this = ".this$" in
      let last_char = fn.[fn_len - 1] in
      (last_char >= '0' && last_char <= '9')
      && String.is_suffix fn ~suffix:(this ^ String.of_char last_char)
  end
end

module Struct = struct
  type field = Fieldname.t * T.t * Annot.Item.t [@@deriving compare]

  type fields = field list

  (** Type for a structured value. *)
  type t =
    { fields: fields  (** non-static fields *)
    ; statics: fields  (** static fields *)
    ; supers: Name.t list  (** superclasses *)
    ; methods: Procname.t list  (** methods defined *)
    ; annots: Annot.Item.t  (** annotations *) }

  type lookup = Name.t -> t option

  let pp_field pe f (field_name, typ, ann) =
    F.fprintf f "@\n\t\t%a %a %a" (pp_full pe) typ Fieldname.pp field_name Annot.Item.pp ann


  let pp pe name f {fields; supers; methods; annots} =
    if Config.debug_mode then
      (* change false to true to print the details of struct *)
      F.fprintf f
        "%a @\n\
         \tfields: {%a@\n\
         \t}@\n\
         \tsupers: {%a@\n\
         \t}@\n\
         \tmethods: {%a@\n\
         \t}@\n\
         \tannots: {%a@\n\
         \t}" Name.pp name
        (Pp.seq (pp_field pe))
        fields
        (Pp.seq (fun f n -> F.fprintf f "@\n\t\t%a" Name.pp n))
        supers
        (Pp.seq (fun f m -> F.fprintf f "@\n\t\t%a" Procname.pp m))
        methods Annot.Item.pp annots
    else F.fprintf f "%a" Name.pp name


  let internal_mk_struct ?default ?fields ?statics ?methods ?supers ?annots () =
    let default_ = {fields= []; statics= []; methods= []; supers= []; annots= Annot.Item.empty} in
    let mk_struct_ ?(default= default_) ?(fields= default.fields) ?(statics= default.statics)
        ?(methods= default.methods) ?(supers= default.supers) ?(annots= default.annots) () =
      {fields; statics; methods; supers; annots}
    in
    mk_struct_ ?default ?fields ?statics ?methods ?supers ?annots ()


  (** the element typ of the final extensible array in the given typ, if any *)
  let rec get_extensible_array_element_typ ~lookup (typ: T.t) =
    match typ.desc with
    | Tarray {elt} ->
        Some elt
    | Tstruct name -> (
      match lookup name with
      | Some {fields} -> (
        match List.last fields with
        | Some (_, fld_typ, _) ->
            get_extensible_array_element_typ ~lookup fld_typ
        | None ->
            None )
      | None ->
          None )
    | _ ->
        None


  (** If a struct type with field f, return the type of f. If not, return the default *)
  let fld_typ ~lookup ~default fn (typ: T.t) =
    match typ.desc with
    | Tstruct name -> (
      match lookup name with
      | Some {fields} ->
          List.find ~f:(fun (f, _, _) -> Fieldname.equal f fn) fields
          |> Option.value_map ~f:snd3 ~default
      | None ->
          default )
    | _ ->
        default


  let get_field_type_and_annotation ~lookup fn (typ: T.t) =
    match typ.desc with
    | Tstruct name | Tptr ({desc= Tstruct name}, _) -> (
      match lookup name with
      | Some {fields; statics} ->
          List.find_map
            ~f:(fun (f, t, a) ->
              match Fieldname.equal f fn with true -> Some (t, a) | false -> None )
            (fields @ statics)
      | None ->
          None )
    | _ ->
        None
end
