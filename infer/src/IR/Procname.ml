(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Hashtbl = Caml.Hashtbl
module F = Format
module L = Logging

type detail_level = FullNameOnly | NameOnly | Non_verbose | Simple | Verbose

let is_verbose v = match v with Verbose -> true | _ -> false

let remove_templates name =
  match String.lsplit2 ~on:'<' name with
  | Some (name_without_template, _template_part) ->
      name_without_template
  | None ->
      name


module CSharp = struct
  type kind = Non_Static | Static [@@deriving compare, equal, yojson_of, sexp, hash, normalize]

  type t =
    { method_name: string
    ; parameters: Typ.t list
    ; class_name: Typ.Name.t
    ; return_type: Typ.t option (* option because constructors have no return type *)
    ; kind: kind }
  [@@deriving compare, equal, yojson_of, sexp, hash, normalize]

  let ensure_csharp_type t =
    if not (Typ.is_csharp_type t) then
      L.die InternalError "Expected csharp type but got %a@." (Typ.pp_full Pp.text) t


  let make ~class_name ~return_type ~method_name ~parameters ~kind () =
    Option.iter return_type ~f:ensure_csharp_type ;
    {class_name; return_type; method_name; parameters; kind}


  let pp_return_type ~verbose fmt j = Option.iter j.return_type ~f:(Typ.pp_cs ~verbose fmt)

  let constructor_method_name = ".ctor"

  let get_class_name cs = Typ.Name.name cs.class_name

  let get_class_type_name cs = cs.class_name

  let get_csharp_class_name_exn cs =
    match cs.class_name with
    | Typ.CSharpClass csharp_class_name ->
        csharp_class_name
    | _ ->
        L.die InternalError "Asked for csharp class name but got something else"


  let get_simple_class_name cs = CSharpClassName.classname (get_csharp_class_name_exn cs)

  let get_method cs = cs.method_name

  let get_return_typ pname_csharp = Option.value ~default:StdTyp.void pname_csharp.return_type

  let replace_parameters parameters cs = {cs with parameters}

  let get_parameters cs = cs.parameters

  let is_generated {method_name} = String.is_prefix ~prefix:"$" method_name

  (** Prints a string of a csharp procname with the given level of verbosity *)
  let pp ?(withclass = false) verbosity fmt cs =
    let verbose = is_verbose verbosity in
    let pp_class_name_dot ~with_namespace fmt cs =
      CSharpClassName.pp_with_verbosity ~verbose:with_namespace fmt (get_csharp_class_name_exn cs) ;
      F.pp_print_char fmt '.'
    in
    let pp_package_method_and_params ~with_namespace fmt cs =
      let pp_param_list fmt params = Pp.seq ~sep:"," (Typ.pp_cs ~verbose) fmt params in
      F.fprintf fmt "%a%s(%a)"
        (pp_class_name_dot ~with_namespace)
        cs cs.method_name pp_param_list cs.parameters
    in
    match verbosity with
    | Verbose ->
        (* [namespace.class.method(params): rtype], used for example to create unique filenames *)
        let separator = if Option.is_none cs.return_type then "" else ":" in
        pp_package_method_and_params ~with_namespace:true fmt cs ;
        F.fprintf fmt "%s%a" separator (pp_return_type ~verbose) cs
    | Non_verbose ->
        (* [rtype class.method(params)], for creating reports *)
        let separator = if Option.is_none cs.return_type then "" else " " in
        F.fprintf fmt "%a%s" (pp_return_type ~verbose) cs separator ;
        pp_package_method_and_params ~with_namespace:false fmt cs
    | Simple ->
        (* [methodname(...)] or without ... if there are no parameters *)
        let params = match cs.parameters with [] -> "" | _ -> "..." in
        let pp_method_name fmt cs =
          if String.equal cs.method_name constructor_method_name then
            F.pp_print_string fmt (get_simple_class_name cs)
          else (
            if withclass then pp_class_name_dot ~with_namespace:false fmt cs ;
            F.pp_print_string fmt cs.method_name )
        in
        F.fprintf fmt "%a(%s)" pp_method_name cs params
    | FullNameOnly ->
        (* [namespace.class.method], for name matching *)
        F.fprintf fmt "%a%s" (pp_class_name_dot ~with_namespace:true) cs cs.method_name
    | NameOnly ->
        (* [package.class.method], for name matching *)
        F.fprintf fmt "%a%s" (pp_class_name_dot ~with_namespace:false) cs cs.method_name
end

module Java = struct
  type kind =
    | Non_Static
        (** in Java, procedures called with invokevirtual, invokespecial, and invokeinterface *)
    | Static  (** in Java, procedures called with invokestatic *)
  [@@deriving compare, equal, yojson_of, sexp, hash, normalize]

  (** Type of java procedure names. *)
  type t =
    { method_name: string
    ; parameters: Typ.t list
    ; class_name: Typ.Name.t
    ; return_type: Typ.t option (* option because constructors have no return type *)
    ; kind: kind }
  [@@deriving compare, equal, yojson_of, sexp, hash, normalize]

  let ensure_java_type t =
    if not (Typ.is_java_type t) then
      L.die InternalError "Expected java type but got %a@." (Typ.pp_full Pp.text) t


  let make ~class_name ~return_type ~method_name ~parameters ~kind () =
    Option.iter return_type ~f:ensure_java_type ;
    {class_name; return_type; method_name; parameters; kind}


  let pp_return_type ~verbose fmt j = Option.iter j.return_type ~f:(Typ.pp_java ~verbose fmt)

  let constructor_method_name = "<init>"

  let class_initializer_method_name = "<clinit>"

  let get_class_name j = Typ.Name.name j.class_name

  let get_class_type_name j = j.class_name

  let get_java_class_name_exn j =
    match j.class_name with
    | Typ.JavaClass java_class_name ->
        java_class_name
    | _ ->
        L.die InternalError "Asked for java class name but got something else"


  let get_simple_class_name j = JavaClassName.classname (get_java_class_name_exn j)

  let get_package j = JavaClassName.package (get_java_class_name_exn j)

  let get_method j = j.method_name

  let replace_parameters parameters j = {j with parameters}

  let get_parameters j = j.parameters

  (** Prints a string of a java procname with the given level of verbosity *)
  let pp ?(withclass = false) verbosity fmt j =
    let verbose = is_verbose verbosity in
    let pp_class_name_dot ~with_package fmt j =
      JavaClassName.pp_with_verbosity ~verbose:with_package fmt (get_java_class_name_exn j) ;
      F.pp_print_char fmt '.'
    in
    let pp_package_method_and_params ~with_package fmt j =
      let pp_param_list fmt params = Pp.seq ~sep:"," (Typ.pp_java ~verbose) fmt params in
      F.fprintf fmt "%a%s(%a)" (pp_class_name_dot ~with_package) j j.method_name pp_param_list
        j.parameters
    in
    match verbosity with
    | Verbose ->
        (* [package.class.method(params): rtype], used for example to create unique filenames *)
        let separator = if Option.is_none j.return_type then "" else ":" in
        pp_package_method_and_params ~with_package:true fmt j ;
        F.fprintf fmt "%s%a" separator (pp_return_type ~verbose) j
    | Non_verbose ->
        (* [rtype class.method(params)], for creating reports *)
        let separator = if Option.is_none j.return_type then "" else " " in
        F.fprintf fmt "%a%s" (pp_return_type ~verbose) j separator ;
        pp_package_method_and_params ~with_package:false fmt j
    | Simple ->
        let params = match j.parameters with [] -> "" | _ -> "..." in
        (* [methodname(...)] or without ... if there are no parameters *)
        let pp_method_name fmt j =
          if String.equal j.method_name constructor_method_name then
            F.pp_print_string fmt (get_simple_class_name j)
          else (
            if withclass then pp_class_name_dot ~with_package:false fmt j ;
            F.pp_print_string fmt j.method_name )
        in
        F.fprintf fmt "%a(%s)" pp_method_name j params
    | FullNameOnly ->
        (* [package.class.method], for name matching *)
        F.fprintf fmt "%a%s" (pp_class_name_dot ~with_package:true) j j.method_name
    | NameOnly ->
        (* [class.method], for simple name matching *)
        F.fprintf fmt "%a%s" (pp_class_name_dot ~with_package:false) j j.method_name


  let get_return_typ pname_java = Option.value ~default:StdTyp.void pname_java.return_type

  let is_close {method_name} = String.equal method_name "close"

  let is_class_initializer {method_name} = String.equal method_name class_initializer_method_name

  let get_class_initializer class_name =
    { method_name= class_initializer_method_name
    ; parameters= []
    ; class_name
    ; return_type= Some StdTyp.void
    ; kind= Static }


  let is_constructor {method_name} = String.equal method_name constructor_method_name

  let is_anonymous_inner_class_method {class_name} =
    Option.value ~default:false (Typ.Name.Java.is_anonymous_inner_class_name_opt class_name)


  let is_static {kind} = match kind with Static -> true | _ -> false

  let is_instance x = not (is_static x)

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


  let is_autogen_method {method_name} = JConfig.is_synthetic_name method_name

  let is_external java_pname =
    let package = get_package java_pname in
    Option.exists ~f:Config.java_package_is_external package
end

module Parameter = struct
  (** Type for parameters in clang procnames, [Some name] means the parameter is of type pointer to
      struct, with [name] being the name of the struct, [None] means the parameter is of some other
      type. *)
  type clang_parameter = Typ.Name.t option
  [@@deriving compare, equal, yojson_of, sexp, hash, normalize]

  (** Type for parameters in procnames, for java and clang. *)
  type t =
    | JavaParameter of Typ.t
    | ClangParameter of clang_parameter
    | CSharpParameter of Typ.t
    | ErlangParameter
  [@@deriving compare, equal, hash, normalize]

  let of_typ typ =
    match typ.Typ.desc with Typ.Tptr ({desc= Tstruct name}, Pk_pointer) -> Some name | _ -> None


  let pp_parameters fmt parameters =
    if List.exists ~f:Option.is_some parameters then
      (* the tests rely on the fact that we discard non-pointer-to-struct types for some reason,
         hence the slight re-implementation of [Pp.seq] to avoid building the list of [Some] items
         explicitly *)
      let rec pp_parameters_aux fmt = function
        | [] ->
            ()
        | [Some param] ->
            F.pp_print_string fmt (Typ.Name.to_string param)
        | None :: parameters ->
            pp_parameters_aux fmt parameters
        | (Some _ as param_some) :: None :: parameters ->
            pp_parameters_aux fmt (param_some :: parameters)
        | Some param :: (Some _ :: _ as parameters) ->
            F.fprintf fmt "%s," (Typ.Name.to_string param) ;
            pp_parameters_aux fmt parameters
      in
      F.fprintf fmt "(%a)" pp_parameters_aux parameters


  let clang_param_of_name class_name : clang_parameter = Some class_name
end

module ObjC_Cpp = struct
  type mangled = string option [@@deriving compare, equal, yojson_of, sexp, hash, normalize]

  type kind =
    | CPPMethod of mangled
    | CPPConstructor of mangled
    | CPPDestructor of mangled
    | ObjCClassMethod
    | ObjCInstanceMethod
  [@@deriving compare, equal, yojson_of, sexp, hash, normalize]

  type t =
    { class_name: Typ.Name.t
    ; kind: kind
    ; method_name: string
    ; parameters: Parameter.clang_parameter list
    ; template_args: Typ.template_spec_info }
  [@@deriving compare, equal, yojson_of, sexp, hash, normalize]

  let make class_name method_name kind template_args parameters =
    {class_name; method_name; kind; template_args; parameters}


  let make_dealloc name = make name "dealloc" ObjCInstanceMethod Typ.NoTemplate []

  let make_copy name = make name "copy" ObjCInstanceMethod Typ.NoTemplate []

  let make_copyWithZone ~is_mutable name =
    let zone = Typ.CStruct (QualifiedCppName.of_qual_string "_NSZone") in
    let method_name = if is_mutable then "mutableCopyWithZone:" else "copyWithZone:" in
    make name method_name ObjCInstanceMethod Typ.NoTemplate [Parameter.clang_param_of_name zone]


  let get_class_name objc_cpp = Typ.Name.name objc_cpp.class_name

  let get_class_type_name objc_cpp = objc_cpp.class_name

  let get_class_qualifiers objc_cpp = Typ.Name.qual_name objc_cpp.class_name

  let objc_method_kind_of_bool is_instance =
    if is_instance then ObjCInstanceMethod else ObjCClassMethod


  let is_prefix_init s = String.is_prefix ~prefix:"init" s

  let is_objc_constructor method_name = String.equal method_name "new" || is_prefix_init method_name

  let is_objc_kind = function ObjCClassMethod | ObjCInstanceMethod -> true | _ -> false

  let is_objc_method {kind} = is_objc_kind kind

  let is_objc_dealloc method_name = String.equal method_name "dealloc"

  let is_destructor = function
    | {kind= CPPDestructor _} ->
        true
    | name ->
        is_objc_dealloc name.method_name


  let is_inner_destructor ({method_name} as pname) =
    is_destructor pname && String.is_prefix ~prefix:Config.clang_inner_destructor_prefix method_name


  let is_cpp_lambda {method_name} = String.is_substring ~substring:"operator()" method_name

  let pp_verbose_kind fmt = function
    | CPPMethod mangled | CPPDestructor mangled ->
        F.fprintf fmt "(%s)" (Option.value ~default:"" mangled)
    | CPPConstructor mangled ->
        F.fprintf fmt "{%s}" (Option.value ~default:"" mangled)
    | ObjCClassMethod ->
        F.pp_print_string fmt "[class]"
    | ObjCInstanceMethod ->
        F.pp_print_string fmt "[instance]"


  let get_sep osig = if is_objc_method osig then "." else "::"

  let pp verbosity fmt osig =
    let sep = get_sep osig in
    match verbosity with
    | Simple ->
        F.pp_print_string fmt osig.method_name
    | Non_verbose | NameOnly ->
        F.fprintf fmt "%s%s%s" (Typ.Name.name osig.class_name) sep osig.method_name
    | FullNameOnly ->
        F.fprintf fmt "%a%s%s" Typ.Name.pp osig.class_name sep osig.method_name
    | Verbose ->
        F.fprintf fmt "%a%s%s%a%a" Typ.Name.pp osig.class_name sep osig.method_name
          Parameter.pp_parameters osig.parameters pp_verbose_kind osig.kind


  let pp_without_templates fmt osig =
    F.fprintf fmt "%s%s%s"
      (Typ.Name.name_without_templates osig.class_name)
      (get_sep osig)
      (remove_templates osig.method_name)


  let get_parameters osig = osig.parameters

  let replace_parameters new_parameters osig = {osig with parameters= new_parameters}
end

module C = struct
  (** Type of c procedure names. *)
  type t = Typ.c_function_sig [@@deriving compare, equal, yojson_of, sexp, hash, normalize]

  let c c_name ?mangled template_args =
    {Typ.c_name; c_mangled= mangled; c_template_args= template_args}


  let from_string name =
    {Typ.c_name= QualifiedCppName.of_qual_string name; c_mangled= None; c_template_args= NoTemplate}


  let pp verbosity fmt {Typ.c_name; c_mangled} =
    let plain = QualifiedCppName.to_qual_string c_name in
    match verbosity with
    | Simple ->
        F.fprintf fmt "%s()" plain
    | Non_verbose | FullNameOnly | NameOnly ->
        F.pp_print_string fmt plain
    | Verbose ->
        let pp_mangled fmt = function None -> () | Some s -> F.fprintf fmt "{%s}" s in
        F.fprintf fmt "%s%a" plain pp_mangled c_mangled


  let pp_without_templates fmt {Typ.c_name} =
    let plain = QualifiedCppName.to_qual_string c_name in
    F.pp_print_string fmt (remove_templates plain)


  (** NOTE: [std::_] is parsed as [C] proc name in Sil, rather than [ObjC_Cpp]. *)
  let is_std_function ~prefix {Typ.c_name} =
    match QualifiedCppName.to_rev_list c_name with
    | [fname; "std"] when String.is_prefix fname ~prefix ->
        true
    | _ ->
        false


  let is_make_shared c = is_std_function ~prefix:"make_shared" c

  let is_std_move c = is_std_function ~prefix:"move" c
end

module Erlang = struct
  type t = {module_name: string; function_name: string; arity: int}
  [@@deriving compare, equal, yojson_of, sexp, hash, normalize]

  let pp_general arity_sep verbosity fmt {module_name; function_name; arity} =
    match verbosity with
    | Simple | Non_verbose ->
        F.fprintf fmt "%s%c%d" function_name arity_sep arity
    | Verbose ->
        F.fprintf fmt "%s:%s%c%d" module_name function_name arity_sep arity
    | FullNameOnly | NameOnly ->
        F.fprintf fmt "%s:%s" module_name function_name


  let pp verbosity fmt pname = pp_general '/' verbosity fmt pname

  let pp_filename fmt {module_name; function_name; arity} =
    (* Extend list of illegal characters if needed. *)
    let invalid_chars = "/:<>" in
    let sanitize_char chr =
      if String.mem invalid_chars chr then Printf.sprintf "%#x" (Char.to_int chr)
      else Char.to_string chr
    in
    let sanitize str = String.concat_map str ~f:sanitize_char in
    let module_name = sanitize module_name in
    let function_name = sanitize function_name in
    pp_general '#' Verbose fmt {module_name; function_name; arity}


  let set_arity arity name = {name with arity}

  let call_unqualified_function_name = "__call_unqualified"

  let call_qualified_function_name = "__call_qualified"

  let call_unqualified fun_arity =
    { module_name= ErlangTypeName.infer_erlang_namespace
    ; function_name= call_unqualified_function_name
    ; arity= fun_arity + 1 }


  let is_call_unqualified {module_name; function_name; _} =
    String.equal module_name ErlangTypeName.infer_erlang_namespace
    && String.equal function_name call_unqualified_function_name


  let call_qualified fun_arity =
    { module_name= ErlangTypeName.infer_erlang_namespace
    ; function_name= call_qualified_function_name
    ; arity= fun_arity + 2 }


  let is_call_qualified {module_name; function_name; _} =
    String.equal module_name ErlangTypeName.infer_erlang_namespace
    && String.equal function_name call_qualified_function_name
end

module Block = struct
  (** Type of Objective C block names. *)

  type t = Typ.objc_block_sig [@@deriving compare, equal, yojson_of, sexp, hash, normalize]

  let pp verbosity fmt (bsig : Typ.objc_block_sig) =
    match verbosity with
    | Simple | FullNameOnly | NameOnly ->
        F.pp_print_string fmt "block"
    | Non_verbose ->
        F.fprintf fmt "%s" bsig.name
    | Verbose ->
        F.fprintf fmt "%s_%s" bsig.name bsig.mangled


  let get_class_type_name ({class_name} : Typ.objc_block_sig) = class_name

  let get_class_name block = get_class_type_name block |> Option.map ~f:Typ.Name.name
end

module Hack = struct
  type t = {class_name: HackClassName.t option; function_name: string; arity: int option}
  [@@deriving compare, equal, yojson_of, sexp, hash, normalize]

  let get_class_type_name {class_name} = Option.map class_name ~f:(fun cn -> Typ.HackClass cn)

  let get_arity {arity} = arity

  let r = Str.regexp {|gen\(\(a\|m\|v\)\(k\|w\|kw\)?\|x\|vx\)?\([A-Z]\([0-9a-zA-Z]*\)\)?$|}

  let is_named_genx {function_name} = Str.string_match r function_name 0
  (* recognizes "gen", optionally followed by one of the following, optionally followed by an identifier that starts with a capital letter
     a, ak, aw, akw,
     v, vk, vw, vkw,
     m, mk, mw, mkw,
     x, vx,
  *)

  let is_vec_or_dict_from_async {function_name} =
    String.equal function_name "FlibSL::Vec::from_async"
    || String.equal function_name "FlibSL::Dict::from_async"


  let pp verbosity fmt t =
    let pp_arity verbosity fmt =
      match verbosity with
      | Verbose -> (
        match t.arity with Some arity -> F.fprintf fmt "#%d" arity | None -> () )
      | Non_verbose | Simple | FullNameOnly | NameOnly ->
          ()
    in
    match verbosity with
    | NameOnly ->
        F.fprintf fmt "%s" t.function_name
    | FullNameOnly | Simple | Non_verbose | Verbose -> (
      match t.class_name with
      | Some class_name ->
          F.fprintf fmt "%a.%s%t" HackClassName.pp class_name t.function_name (pp_arity verbosity)
      | _ ->
          F.fprintf fmt "%s%t" t.function_name (pp_arity verbosity) )


  let get_class_name_as_a_string {class_name} = Option.map class_name ~f:HackClassName.classname

  let get_static_init ~is_trait class_name =
    let static_class_name = HackClassName.static_companion class_name in
    let arity = if is_trait then 2 else 1 in
    {class_name= Some static_class_name; function_name= "_86sinit"; arity= Some arity}


  let is_xinit {function_name= name} =
    String.equal name "_86sinit" || String.equal name "_86pinit" || String.equal name "_86cinit"
end

module Python = struct
  let init_name = "__init__"

  type t = {class_name: PythonClassName.t option; function_name: string; arity: int option}
  [@@deriving compare, equal, yojson_of, sexp, hash, normalize]

  let get_class_type_name {class_name} = Option.map class_name ~f:(fun cn -> Typ.PythonClass cn)

  type kind = Fun of PythonClassName.t | Init of PythonClassName.t | Other

  let classify {class_name; function_name} =
    match class_name with
    | Some class_name ->
        if String.equal function_name init_name then Init class_name else Other
    | None ->
        Fun (PythonClassName.make function_name)


  (* This function is used to transform a "constructor" call like [MyClass(x, y)] into a call to
     the [__init__] function, like [foo.__init__(x, y)]. Therefore must increase arity by 1, to
     account for [__init__] being a virtual call expecting a [self] argument. *)
  let mk_init {class_name; function_name; arity} =
    match class_name with
    | Some _ ->
        L.die InternalError "Procname.Python.mk_init expects a top level procname"
    | None ->
        let class_name = Some (PythonClassName.make function_name) in
        let arity = Option.map ~f:(fun n -> 1 + n) arity in
        {class_name; function_name= init_name; arity}


  let pp verbosity fmt t =
    let pp_arity verbosity fmt =
      match verbosity with
      | Verbose -> (
        match t.arity with Some arity -> F.fprintf fmt "#%d" arity | None -> () )
      | Non_verbose | Simple | FullNameOnly | NameOnly ->
          ()
    in
    match verbosity with
    | NameOnly ->
        F.fprintf fmt "%s" t.function_name
    | FullNameOnly | Simple | Non_verbose | Verbose -> (
      match t.class_name with
      | Some class_name ->
          F.fprintf fmt "%a.%s%t" PythonClassName.pp class_name t.function_name (pp_arity verbosity)
      | _ ->
          F.fprintf fmt "%s%t" t.function_name (pp_arity verbosity) )
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
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

let is_c = function C _ -> true | _ -> false

let is_erlang_unsupported name =
  match name with
  | Erlang {module_name; _} ->
      String.equal module_name ErlangTypeName.unsupported
  | _ ->
      false


let is_hack_async_name name =
  match name with
  | Hack hack_name ->
      Hack.is_named_genx hack_name || Hack.is_vec_or_dict_from_async hack_name
  | _ ->
      false


let is_erlang_call_unqualified name =
  match name with Erlang erlang_name -> Erlang.is_call_unqualified erlang_name | _ -> false


let is_erlang_call_qualified name =
  match name with Erlang erlang_name -> Erlang.is_call_qualified erlang_name | _ -> false


let is_erlang = function Erlang _ -> true | _ -> false

let compare_name x y =
  let open ICompare in
  match (x, y) with
  | ( CSharp {class_name= class_name1; method_name= method_name1}
    , CSharp {class_name= class_name2; method_name= method_name2} )
  | ( Java {class_name= class_name1; method_name= method_name1}
    , Java {class_name= class_name2; method_name= method_name2} )
  | ( ObjC_Cpp {class_name= class_name1; method_name= method_name1}
    , ObjC_Cpp {class_name= class_name2; method_name= method_name2} ) ->
      Typ.Name.compare_name class_name1 class_name2
      <*> fun () -> String.compare method_name1 method_name2
  | CSharp _, _ ->
      -1
  | _, CSharp _ ->
      1
  | Java _, _ ->
      -1
  | _, Java _ ->
      1
  | C {c_name= name1}, C {c_name= name2} ->
      QualifiedCppName.compare_name name1 name2
  | C _, _ ->
      -1
  | _, C _ ->
      1
  | Erlang name1, Erlang name2 ->
      Erlang.compare name1 name2
  | Erlang _, _ ->
      -1
  | _, Erlang _ ->
      1
  | Hack name1, Hack name2 ->
      Hack.compare name1 name2
  | Hack _, _ ->
      -1
  | _, Hack _ ->
      1
  | Block _, Block _ ->
      0
  | Block _, _ ->
      -1
  | _, Block _ ->
      1
  | ObjC_Cpp _, _ ->
      -1
  | _, ObjC_Cpp _ ->
      1
  | Python name1, Python name2 ->
      Python.compare name1 name2


let is_std_move t = match t with C c_pname -> C.is_std_move c_pname | _ -> false

let is_cpp_assignment_operator t =
  match t with ObjC_Cpp name when String.equal name.method_name "operator=" -> true | _ -> false


let is_destructor t =
  match t with ObjC_Cpp objc_cpp_pname -> ObjC_Cpp.is_destructor objc_cpp_pname | _ -> false


let is_csharp t = match t with CSharp _ -> true | _ -> false

let is_hack t = match t with Hack _ -> true | _ -> false

let is_java t = match t with Java _ -> true | _ -> false

let is_python t = match t with Python _ -> true | _ -> false

(* TODO: deprecate this unfortunately named function and use is_clang instead *)
let is_c_method t = match t with ObjC_Cpp _ -> true | _ -> false

let is_clang t = match t with ObjC_Cpp _ | C _ -> true | _ -> false

let is_java_lift f t = match t with Java java_pname -> f java_pname | _ -> false

let is_java_static_method = is_java_lift Java.is_static

let is_java_instance_method = is_java_lift Java.is_instance

let is_java_access_method = is_java_lift Java.is_access_method

let is_java_class_initializer = is_java_lift Java.is_class_initializer

let is_java_anonymous_inner_class_method = is_java_lift Java.is_anonymous_inner_class_method

let is_java_autogen_method = is_java_lift Java.is_autogen_method

let on_objc_helper ~f ~default = function
  | ObjC_Cpp objc_cpp_pname ->
      f objc_cpp_pname
  | Block _ | C _ | CSharp _ | Erlang _ | Hack _ | Java _ | Python _ ->
      default


let is_objc_helper ~f proc_name = on_objc_helper ~f ~default:false proc_name

let get_objc_helper ~f proc_name = on_objc_helper ~f ~default:None proc_name

let is_objc_method = is_objc_helper ~f:ObjC_Cpp.is_objc_method

let is_objc_dealloc =
  is_objc_helper ~f:(fun objc_cpp_pname ->
      ObjC_Cpp.is_objc_method objc_cpp_pname && ObjC_Cpp.is_objc_dealloc objc_cpp_pname.method_name )


let is_objc_init =
  is_objc_helper ~f:(fun objc_cpp_pname ->
      ObjC_Cpp.is_objc_method objc_cpp_pname && ObjC_Cpp.is_prefix_init objc_cpp_pname.method_name )


let is_objc_instance_method =
  is_objc_helper ~f:(function {kind= ObjCInstanceMethod} -> true | _ -> false)


let is_objc_class_method =
  is_objc_helper ~f:(function {kind= ObjCClassMethod} -> true | _ -> false)


let is_objc_nsobject_class =
  is_objc_helper ~f:(function {class_name} -> String.equal (Typ.Name.name class_name) "NSObject")


let get_objc_class_name proc_name =
  get_objc_helper proc_name ~f:(fun objc_cpp_pname ->
      if ObjC_Cpp.is_objc_method objc_cpp_pname then Some (ObjC_Cpp.get_class_name objc_cpp_pname)
      else None )


let empty_block = Block {class_name= None; name= ""; mangled= ""}

(** Replace the class name component of a procedure name. In case of Java, replace package and class
    name. For Hack traits, we also update their arity. [hackc] introduces a new parameter to each
    method in a trait. Therefore when we compare a method from a class and a method from a trait,
    their arity won't match even if it was the case in the original Hack source file. *)
let replace_class t ?(arity_incr = 0) (new_class : Typ.Name.t) =
  match t with
  | Java j ->
      Java {j with class_name= new_class}
  | CSharp cs ->
      CSharp {cs with class_name= new_class}
  | ObjC_Cpp osig ->
      ObjC_Cpp {osig with class_name= new_class}
  | Hack ({arity} as h) ->
      let arity = Option.map ~f:(fun arity -> arity + arity_incr) arity in
      let name =
        match new_class with
        | HackClass name ->
            name
        | _ ->
            L.die InternalError "replace_class on ill-formed Hack type"
      in
      Hack {h with class_name= Some name; arity}
  | Python p ->
      let name =
        match new_class with
        | PythonClass name ->
            name
        | _ ->
            L.die InternalError "replace_class on ill-formed Python type"
      in
      Python {p with class_name= Some name}
  | C _ | Block _ | Erlang _ ->
      t


let get_class_type_name t =
  match t with
  | Java java_pname ->
      Some (Java.get_class_type_name java_pname)
  | CSharp cs_pname ->
      Some (CSharp.get_class_type_name cs_pname)
  | ObjC_Cpp objc_pname ->
      Some (ObjC_Cpp.get_class_type_name objc_pname)
  | Block bsig ->
      Block.get_class_type_name bsig
  | Hack hack ->
      Hack.get_class_type_name hack
  | Python python ->
      Python.get_class_type_name python
  | C _ | Erlang _ ->
      None


let get_class_name t =
  match t with
  | Java java_pname ->
      Some (Java.get_class_name java_pname)
  | CSharp cs_pname ->
      Some (CSharp.get_class_name cs_pname)
  | ObjC_Cpp objc_pname ->
      Some (ObjC_Cpp.get_class_name objc_pname)
  | Block bsig ->
      Block.get_class_name bsig
  | Hack hack_pname ->
      Hack.get_class_name_as_a_string hack_pname
  | Python _ ->
      L.die InternalError "TODO: get_class_name for Python type"
  | C _ | Erlang _ ->
      None


let python_classify = function Python p -> Some (Python.classify p) | _ -> None

let mk_python_init = function
  | Python p ->
      Python (Python.mk_init p)
  | _ ->
      L.die InternalError "Procname.mk_python_init only supports Python names"


let is_method_in_objc_protocol t =
  match t with ObjC_Cpp osig -> Typ.Name.is_objc_protocol osig.class_name | _ -> false


let objc_cpp_replace_method_name t (new_method_name : string) =
  match t with
  | ObjC_Cpp osig ->
      ObjC_Cpp {osig with method_name= new_method_name}
  | C _ | CSharp _ | Block _ | Erlang _ | Hack _ | Java _ | Python _ ->
      t


(** Return the method/function of a procname. *)
let get_method = function
  | ObjC_Cpp name ->
      name.method_name
  | C {c_name} ->
      QualifiedCppName.to_qual_string c_name
  | Erlang name ->
      name.function_name
  | Hack name ->
      name.function_name
  | Block bsig ->
      bsig.name
  | Java j ->
      j.method_name
  | CSharp cs ->
      cs.method_name
  | Python name ->
      name.function_name


(** Return whether the procname is a block procname. *)
let is_objc_block = function Block _ -> true | _ -> false

(** Return whether the procname is a cpp lambda procname. *)
let is_cpp_lambda t =
  match t with ObjC_Cpp cpp_pname when ObjC_Cpp.is_cpp_lambda cpp_pname -> true | _ -> false


(** Return the language of the procedure. *)
let get_language = function
  | ObjC_Cpp _ ->
      Language.Clang
  | C _ ->
      Language.Clang
  | Erlang _ ->
      Language.Erlang
  | Hack _ ->
      Language.Hack
  | Block _ ->
      Language.Clang
  | Java _ ->
      Language.Java
  | CSharp _ ->
      Language.CIL
  | Python _ ->
      Language.Python


(** [is_constructor pname] returns true if [pname] is a constructor *)
let is_constructor t =
  match t with
  | CSharp c ->
      String.equal c.method_name CSharp.constructor_method_name
  | Java js ->
      String.equal js.method_name Java.constructor_method_name
  | ObjC_Cpp {kind= CPPConstructor _} ->
      true
  | ObjC_Cpp {kind; method_name} when ObjC_Cpp.is_objc_kind kind ->
      ObjC_Cpp.is_objc_constructor method_name
  | _ ->
      false


(** [is_infer_undefined pn] returns true if [pn] is a special Infer undefined proc *)
let is_infer_undefined = function
  | Java j ->
      String.equal "com.facebook.infer.builtins.InferUndefined" (Java.get_class_name j)
  | _ ->
      (* TODO: add cases for obj-c, c, c++ *)
      false


let is_static = function
  | CSharp {kind= Static} | Java {kind= Static} | ObjC_Cpp {kind= ObjCClassMethod} ->
      Some true
  | CSharp {kind= Non_Static} | Java {kind= Non_Static} | ObjC_Cpp {kind= ObjCInstanceMethod} ->
      Some false
  | C _
  | Block _
  | Erlang _
  | Hack _
  | ObjC_Cpp {kind= CPPMethod _ | CPPConstructor _ | CPPDestructor _}
  | Python _ ->
      None


let is_shared_ptr_observer =
  let observer_methods = ["get"; "operator*"; "operator->"; "operator[]"; "operator_bool"] in
  let aux pname =
    match pname with
    | ObjC_Cpp {class_name= CppClass {name}; method_name} ->
        QualifiedCppName.Match.match_qualifiers Typ.shared_pointer_matcher name
        && List.mem observer_methods method_name ~equal:String.equal
    | _ ->
        false
  in
  fun pname -> aux pname


let is_hack_builtins = function
  | Hack {class_name= Some classname} ->
      HackClassName.is_builtins classname
  | _ ->
      false


let is_hack_sinit = function
  | Hack {function_name} ->
      String.equal function_name "_86sinit"
  | _ ->
      false


let is_hack_construct = function
  | Hack {function_name} ->
      String.equal function_name "__construct"
  | _ ->
      false


let has_hack_classname = function Hack {class_name= Some _} -> true | _ -> false

let get_global_name_of_initializer t =
  match t with
  | C {c_name}
    when String.is_prefix ~prefix:Config.clang_initializer_prefix
           (QualifiedCppName.to_qual_string c_name) ->
      let name_str = QualifiedCppName.to_qual_string c_name in
      let prefix_len = String.length Config.clang_initializer_prefix in
      Some (String.sub name_str ~pos:prefix_len ~len:(String.length name_str - prefix_len))
  | _ ->
      None


let is_lambda_name name =
  String.is_prefix ~prefix:"lambda_" name && String.is_substring ~substring:":" name


let is_lambda = function
  | ObjC_Cpp {class_name} -> (
    match QualifiedCppName.extract_last (Typ.Name.unqualified_name class_name) with
    | Some (name, _) when is_lambda_name name ->
        true
    | _ ->
        false )
  | _ ->
      false


let is_block = function Block _ -> true | _ -> false

let is_lambda_or_block procname = is_lambda procname || is_block procname

(** Very verbose representation of an existing Procname.t *)
let pp_unique_id fmt = function
  | Java j ->
      Java.pp Verbose fmt j
  | CSharp cs ->
      CSharp.pp Verbose fmt cs
  | C osig ->
      C.pp Verbose fmt osig
  | Erlang e ->
      Erlang.pp_filename fmt e
  | Hack h ->
      Hack.pp Verbose fmt h
  | ObjC_Cpp osig ->
      ObjC_Cpp.pp Verbose fmt osig
  | Block bsig ->
      Block.pp Verbose fmt bsig
  | Python h ->
      Python.pp Verbose fmt h


let to_unique_id proc_name = F.asprintf "%a" pp_unique_id proc_name

(** Convert a proc name to a string for the user to see *)
let pp_with_verbosity verbosity fmt = function
  | Java j ->
      Java.pp verbosity fmt j
  | CSharp cs ->
      CSharp.pp verbosity fmt cs
  | C osig ->
      C.pp verbosity fmt osig
  | Erlang e ->
      Erlang.pp verbosity fmt e
  | Hack h ->
      Hack.pp verbosity fmt h
  | ObjC_Cpp osig ->
      ObjC_Cpp.pp verbosity fmt osig
  | Block bsig ->
      Block.pp verbosity fmt bsig
  | Python h ->
      Python.pp verbosity fmt h


let pp = pp_with_verbosity Non_verbose

let pp_verbose = pp_with_verbosity Verbose

let pp_without_templates fmt = function
  | ObjC_Cpp osig ->
      ObjC_Cpp.pp_without_templates fmt osig
  | C csig ->
      C.pp_without_templates fmt csig
  | other ->
      (* For other languages, we use the formaters defined in pp *)
      pp fmt other


let to_string ?(verbosity = Non_verbose) proc_name =
  F.asprintf "%a" (pp_with_verbosity verbosity) proc_name


let to_string_verbose proc_name = to_string ~verbosity:Verbose proc_name

let pp_fullname_only fmt = function
  | Java j ->
      Java.pp FullNameOnly fmt j
  | CSharp cs ->
      CSharp.pp FullNameOnly fmt cs
  | C osig ->
      C.pp FullNameOnly fmt osig
  | Erlang e ->
      Erlang.pp FullNameOnly fmt e
  | Hack h ->
      Hack.pp FullNameOnly fmt h
  | ObjC_Cpp osig ->
      ObjC_Cpp.pp FullNameOnly fmt osig
  | Block bsig ->
      Block.pp FullNameOnly fmt bsig
  | Python h ->
      Python.pp FullNameOnly fmt h


let pp_name_only fmt = function
  | Java j ->
      Java.pp NameOnly fmt j
  | CSharp cs ->
      CSharp.pp NameOnly fmt cs
  | C osig ->
      C.pp NameOnly fmt osig
  | Erlang e ->
      Erlang.pp NameOnly fmt e
  | Hack h ->
      Hack.pp NameOnly fmt h
  | ObjC_Cpp osig ->
      ObjC_Cpp.pp NameOnly fmt osig
  | Block bsig ->
      Block.pp NameOnly fmt bsig
  | Python h ->
      Python.pp NameOnly fmt h


let patterns_match patterns proc_name =
  let s = F.asprintf "%a" pp_name_only proc_name in
  List.exists patterns ~f:(fun pattern -> Str.string_match pattern s 0)


(** Convenient representation of a procname for external tools (e.g. eclipse plugin) *)
let pp_simplified_string ?(withclass = false) fmt = function
  | Java j ->
      Java.pp ~withclass Simple fmt j
  | CSharp cs ->
      CSharp.pp ~withclass Simple fmt cs
  | C osig ->
      C.pp Simple fmt osig
  | Erlang e ->
      Erlang.pp Simple fmt e
  | Hack h ->
      Hack.pp Simple fmt h
  | ObjC_Cpp osig ->
      ObjC_Cpp.pp (if withclass then Non_verbose else Simple) fmt osig
  | Block bsig ->
      Block.pp Simple fmt bsig
  | Python h ->
      Python.pp Simple fmt h


let to_simplified_string ?withclass proc_name =
  F.asprintf "%a" (pp_simplified_string ?withclass) proc_name


let from_string_c_fun func = C (C.from_string func)

let java_inner_class_prefix_regex = Str.regexp "\\$[0-9]+"

let csharp_inner_class_prefix_regex = Str.regexp "\\$[0-9]+"

let replace_regex regex tgt name =
  match Str.search_forward regex name 0 with
  | _ ->
      Str.global_replace regex tgt name
  | exception Caml.Not_found ->
      name


let replace_java_inner_class_prefix_regex = replace_regex java_inner_class_prefix_regex "$_"

let replace_csharp_inner_class_prefix_regex = replace_regex csharp_inner_class_prefix_regex "$_"

let hashable_name proc_name =
  match proc_name with
  | Erlang pname ->
      F.asprintf "%a" (Erlang.pp Verbose) pname
  | Java pname ->
      (* Strip autogenerated anonymous inner class numbers in order to keep the bug hash
         invariant when introducing new anonymous classes *)
      let name = F.asprintf "%a" (Java.pp ~withclass:true Simple) pname in
      replace_java_inner_class_prefix_regex name
  | CSharp pname ->
      let name = F.asprintf "%a" (CSharp.pp ~withclass:true Simple) pname in
      replace_csharp_inner_class_prefix_regex name
  | ObjC_Cpp osig when ObjC_Cpp.is_objc_method osig ->
      (* In Objective C, the list of parameters is part of the method name. To prevent the bug
         hash to change when a parameter is introduced or removed, only the part of the name
         before the first colon is used for the bug hash *)
      let name = F.asprintf "%a" (pp_simplified_string ~withclass:true) proc_name in
      List.hd_exn (String.split name ~on:':')
  | Block bsig ->
      F.asprintf "%a" (Block.pp Non_verbose) bsig
  | _ ->
      (* Other cases for C and C++ method names *)
      F.asprintf "%a" (pp_simplified_string ~withclass:true) proc_name


let get_parameters procname =
  let clang_param_to_param clang_params =
    List.map ~f:(fun par -> Parameter.ClangParameter par) clang_params
  in
  match procname with
  | Java j ->
      List.map ~f:(fun par -> Parameter.JavaParameter par) (Java.get_parameters j)
  | CSharp cs ->
      List.map ~f:(fun par -> Parameter.CSharpParameter par) (CSharp.get_parameters cs)
  | C _ ->
      []
  | Erlang e ->
      List.init e.arity ~f:(fun _ -> Parameter.ErlangParameter)
  | Hack _ ->
      (* TODO(arr): we don't know yet how the parameters of Hack methods will be represented. Will refine later. *)
      []
  | ObjC_Cpp osig ->
      clang_param_to_param (ObjC_Cpp.get_parameters osig)
  | Block _ ->
      []
  | Python _ ->
      (* TODO(vsiles) get inspiration from Hack :D *)
      []


let replace_parameters new_parameters procname =
  let params_to_java_params params =
    List.map
      ~f:(fun param ->
        match param with
        | Parameter.JavaParameter par ->
            par
        | _ ->
            Logging.(die InternalError)
              "Expected Java parameters in Java procname, but got Clang parameters" params )
      params
  in
  let params_to_clang_params params =
    List.map
      ~f:(fun param ->
        match param with
        | Parameter.ClangParameter par ->
            par
        | _ ->
            Logging.(die InternalError)
              "Expected Clang parameters in Clang procname, but got Java parameters" params )
      params
  in
  let params_to_csharp_params params =
    List.map
      ~f:(fun param ->
        match param with
        | Parameter.CSharpParameter par ->
            par
        | _ ->
            Logging.(die InternalError)
              "Expected CSharp parameters in CSharp procname, but got parameters of another \
               language"
              params )
      params
  in
  let params_to_erlang_arity params =
    let check = function
      | Parameter.ErlangParameter ->
          ()
      | _ ->
          L.die InternalError
            "Expected Erlang parameters in Erlang procname, but got parameters of another language"
    in
    List.iter ~f:check params ;
    List.length params
  in
  match procname with
  | Java j ->
      Java (Java.replace_parameters (params_to_java_params new_parameters) j)
  | CSharp cs ->
      CSharp (CSharp.replace_parameters (params_to_csharp_params new_parameters) cs)
  | C _ ->
      procname
  | Erlang e ->
      Erlang (Erlang.set_arity (params_to_erlang_arity new_parameters) e)
  | Hack _ ->
      procname
  | ObjC_Cpp osig ->
      ObjC_Cpp (ObjC_Cpp.replace_parameters (params_to_clang_params new_parameters) osig)
  | Block _ ->
      procname
  | Python _ ->
      procname


let parameter_of_name procname class_name =
  match procname with
  | Java _ ->
      Parameter.JavaParameter Typ.(mk_ptr (mk_struct class_name))
  | CSharp _ ->
      Parameter.CSharpParameter Typ.(mk_ptr (mk_struct class_name))
  | _ ->
      Parameter.ClangParameter (Parameter.clang_param_of_name class_name)


let describe f pn =
  match pn with
  | Block _ | C _ | Erlang _ | Hack _ | Python _ | ObjC_Cpp _ ->
      F.fprintf f "%a()" pp_without_templates pn
  | CSharp _ | Java _ ->
      F.pp_print_string f (hashable_name pn)


let make_java ~class_name ~return_type ~method_name ~parameters ~kind =
  Java (Java.make ~class_name ~return_type ~method_name ~parameters ~kind ())


let make_csharp ~class_name ~return_type ~method_name ~parameters ~kind =
  CSharp (CSharp.make ~class_name ~return_type ~method_name ~parameters ~kind ())


let make_erlang ~module_name ~function_name ~arity = Erlang {module_name; function_name; arity}

let make_hack ~class_name ~function_name ~arity = Hack {class_name; function_name; arity}

let make_objc_dealloc name = ObjC_Cpp (ObjC_Cpp.make_dealloc name)

let make_objc_copy name = ObjC_Cpp (ObjC_Cpp.make_copy name)

let make_objc_copyWithZone ~is_mutable name = ObjC_Cpp (ObjC_Cpp.make_copyWithZone ~is_mutable name)

let make_python ~class_name ~function_name ~arity = Python {class_name; function_name; arity}

let erlang_call_unqualified ~arity = Erlang (Erlang.call_unqualified arity)

let erlang_call_qualified ~arity = Erlang (Erlang.call_qualified arity)

let get_hack_arity = function Hack hack_proc_name -> Hack.get_arity hack_proc_name | _ -> None

let decr_hack_arity procname =
  match procname with
  | Hack ({arity= Some i} as hack_proc_name) when i > 0 ->
      Some (Hack {hack_proc_name with arity= Some (i - 1)})
  | _ ->
      None


let get_hack_static_init ~is_trait class_name = Hack (Hack.get_static_init ~is_trait class_name)

module Hashable = struct
  type nonrec t = t [@@deriving compare, equal]

  let hash = hash

  let sexp_of_t t = Sexp.of_string (to_string t)
end

module Comparable = Comparable.Make (struct
  type nonrec t = t [@@deriving compare, sexp]
end)

include Comparable
module Hash = Hashtbl.Make (Hashable)
module LRUHash = LRUHashtbl.Make (Hashable)
module HashQueue = Hash_queue.Make (Hashable)
module HashSet = HashSet.Make (Hashable)

module Map = PrettyPrintable.MakePPMap (struct
  type nonrec t = t [@@deriving compare]

  let pp = pp
end)

module Set = PrettyPrintable.MakePPSet (struct
  type nonrec t = t [@@deriving compare]

  let pp = pp
end)

let get_qualifiers pname =
  match pname with
  | C {c_name} ->
      c_name
  | ObjC_Cpp objc_cpp ->
      ObjC_Cpp.get_class_qualifiers objc_cpp
      |> QualifiedCppName.append_qualifier ~qual:objc_cpp.method_name
  | _ ->
      QualifiedCppName.empty


let to_short_unique_name pname =
  (* filenames for clang procs are REVERSED qualifiers with '#' as separator *)
  let pp_rev_qualified fmt pname =
    let rev_qualifiers = get_qualifiers pname |> QualifiedCppName.to_rev_list in
    Pp.seq ~sep:"#" F.pp_print_string fmt rev_qualifiers
  in
  let proc_id =
    match pname with
    | C {c_mangled} ->
        let pp_mangled fmt = function None -> () | Some mangled -> F.fprintf fmt "#%s" mangled in
        F.asprintf "%a%a" pp_rev_qualified pname pp_mangled c_mangled
    | ObjC_Cpp objc_cpp ->
        F.asprintf "%a%a#%a" pp_rev_qualified pname Parameter.pp_parameters objc_cpp.parameters
          ObjC_Cpp.pp_verbose_kind objc_cpp.kind
    | _ ->
        F.asprintf "%a" pp_unique_id pname
  in
  DB.append_crc_cutoff proc_id


let to_filename pname = to_short_unique_name pname |> Escape.escape_filename

module SQLite = SqliteUtils.MarshalledDataNOTForComparison (struct
  type nonrec t = t
end)

module SQLiteList = SqliteUtils.MarshalledDataNOTForComparison (struct
  type nonrec t = t list
end)

module UnitCache = struct
  let create () =
    let cache = ref None in
    let cache_get pname =
      Option.bind !cache ~f:(fun (pname', value) -> Option.some_if (equal pname pname') value)
    in
    let cache_set pname value = cache := Some (pname, value) in
    (cache_get, cache_set)
end
