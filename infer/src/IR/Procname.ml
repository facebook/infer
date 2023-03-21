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

(** Level of verbosity of some to_string functions. *)
type detail_level = Verbose | Non_verbose | Simple | NameOnly

let is_verbose v = match v with Verbose -> true | _ -> false

module CSharp = struct
  type kind = Non_Static | Static [@@deriving compare, equal, yojson_of, sexp, hash]

  type t =
    { method_name: string
    ; parameters: Typ.t list
    ; class_name: Typ.Name.t
    ; return_type: Typ.t option (* option because constructors have no return type *)
    ; kind: kind }
  [@@deriving compare, equal, yojson_of, sexp, hash]

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
    let pp_class_name_dot fmt cs =
      CSharpClassName.pp_with_verbosity ~verbose fmt (get_csharp_class_name_exn cs) ;
      F.pp_print_char fmt '.'
    in
    let pp_package_method_and_params fmt cs =
      let pp_param_list fmt params = Pp.seq ~sep:"," (Typ.pp_cs ~verbose) fmt params in
      F.fprintf fmt "%a%s(%a)" pp_class_name_dot cs cs.method_name pp_param_list cs.parameters
    in
    match verbosity with
    | Verbose ->
        (* [package.class.method(params): rtype], used for example to create unique filenames *)
        let separator = if Option.is_none cs.return_type then "" else ":" in
        pp_package_method_and_params fmt cs ;
        F.fprintf fmt "%s%a" separator (pp_return_type ~verbose) cs
    | Non_verbose ->
        (* [rtype class.method(params)], for creating reports *)
        let separator = if Option.is_none cs.return_type then "" else " " in
        F.fprintf fmt "%a%s" (pp_return_type ~verbose) cs separator ;
        pp_package_method_and_params fmt cs
    | Simple ->
        (* [methodname(...)] or without ... if there are no parameters *)
        let params = match cs.parameters with [] -> "" | _ -> "..." in
        let pp_method_name fmt cs =
          if String.equal cs.method_name constructor_method_name then
            F.pp_print_string fmt (get_simple_class_name cs)
          else (
            if withclass then pp_class_name_dot fmt cs ;
            F.pp_print_string fmt cs.method_name )
        in
        F.fprintf fmt "%a(%s)" pp_method_name cs params
    | NameOnly ->
        (* [class.method], for simple name matching *)
        F.fprintf fmt "%a%s" pp_class_name_dot cs cs.method_name
end

module Java = struct
  type kind =
    | Non_Static
        (** in Java, procedures called with invokevirtual, invokespecial, and invokeinterface *)
    | Static  (** in Java, procedures called with invokestatic *)
  [@@deriving compare, equal, yojson_of, sexp, hash]

  (** Type of java procedure names. *)
  type t =
    { method_name: string
    ; parameters: Typ.t list
    ; class_name: Typ.Name.t
    ; return_type: Typ.t option (* option because constructors have no return type *)
    ; kind: kind }
  [@@deriving compare, equal, yojson_of, sexp, hash]

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

  let replace_method_name method_name j = {j with method_name}

  let replace_parameters parameters j = {j with parameters}

  let replace_return_type ret_type j = {j with return_type= Some ret_type}

  let get_parameters j = j.parameters

  (** Prints a string of a java procname with the given level of verbosity *)
  let pp ?(withclass = false) verbosity fmt j =
    let verbose = is_verbose verbosity in
    let pp_class_name_dot fmt j =
      JavaClassName.pp_with_verbosity ~verbose fmt (get_java_class_name_exn j) ;
      F.pp_print_char fmt '.'
    in
    let pp_package_method_and_params fmt j =
      let pp_param_list fmt params = Pp.seq ~sep:"," (Typ.pp_java ~verbose) fmt params in
      F.fprintf fmt "%a%s(%a)" pp_class_name_dot j j.method_name pp_param_list j.parameters
    in
    match verbosity with
    | Verbose ->
        (* [package.class.method(params): rtype], used for example to create unique filenames *)
        let separator = if Option.is_none j.return_type then "" else ":" in
        pp_package_method_and_params fmt j ;
        F.fprintf fmt "%s%a" separator (pp_return_type ~verbose) j
    | Non_verbose ->
        (* [rtype class.method(params)], for creating reports *)
        let separator = if Option.is_none j.return_type then "" else " " in
        F.fprintf fmt "%a%s" (pp_return_type ~verbose) j separator ;
        pp_package_method_and_params fmt j
    | Simple ->
        let params = match j.parameters with [] -> "" | _ -> "..." in
        (* [methodname(...)] or without ... if there are no parameters *)
        let pp_method_name fmt j =
          if String.equal j.method_name constructor_method_name then
            F.pp_print_string fmt (get_simple_class_name j)
          else (
            if withclass then pp_class_name_dot fmt j ;
            F.pp_print_string fmt j.method_name )
        in
        F.fprintf fmt "%a(%s)" pp_method_name j params
    | NameOnly ->
        (* [class.method], for simple name matching *)
        F.fprintf fmt "%a%s" pp_class_name_dot j j.method_name


  let to_simplified_string ?(withclass = false) = Pp.string_of_pp (pp ~withclass Simple)

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

  let is_anonymous_inner_class_constructor_exn {class_name} =
    Typ.Name.Java.is_anonymous_inner_class_name_exn class_name


  let is_anonymous_inner_class_method {class_name} =
    Option.value ~default:false (Typ.Name.Java.is_anonymous_inner_class_name_opt class_name)


  let is_static {kind} = match kind with Static -> true | _ -> false

  let is_instance x = not (is_static x)

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


  let is_autogen_method {method_name} = JConfig.is_synthetic_name method_name

  (** Check if the proc name has the type of a java vararg. Note: currently only checks that the
      last argument has type Object[]. *)
  let is_vararg {parameters} =
    match List.last parameters with
    | Some {desc= Tptr ({desc= Tarray {elt}}, Pk_pointer)} ->
        Typ.equal StdTyp.Java.pointer_to_java_lang_object elt
    | _ ->
        false


  let is_external java_pname =
    let package = get_package java_pname in
    Option.exists ~f:Config.java_package_is_external package


  module Normalizer = HashNormalizer.Make (struct
    type nonrec t = t [@@deriving equal]

    let hash = Hashtbl.hash

    let normalize t =
      let method_name = HashNormalizer.StringNormalizer.normalize t.method_name in
      let parameters =
        IList.map_changed t.parameters ~equal:phys_equal ~f:Typ.Normalizer.normalize
      in
      let class_name = Typ.NameNormalizer.normalize t.class_name in
      let return_type =
        IOption.map_changed t.return_type ~equal:phys_equal ~f:Typ.Normalizer.normalize
      in
      if
        phys_equal method_name t.method_name
        && phys_equal parameters t.parameters
        && phys_equal class_name t.class_name
        && phys_equal return_type t.return_type
      then t
      else {method_name; parameters; class_name; return_type; kind= t.kind}
  end)
end

module Parameter = struct
  (** Type for parameters in clang procnames, [Some name] means the parameter is of type pointer to
      struct, with [name] being the name of the struct, [None] means the parameter is of some other
      type. *)
  type clang_parameter = Typ.Name.t option [@@deriving compare, equal, yojson_of, sexp, hash]

  module ClangParameterNormalizer = HashNormalizer.Make (struct
    type nonrec t = clang_parameter [@@deriving equal]

    let hash = Hashtbl.hash

    let normalize t = IOption.map_changed t ~equal:phys_equal ~f:Typ.NameNormalizer.normalize
  end)

  (** Type for parameters in procnames, for java and clang. *)
  type t =
    | JavaParameter of Typ.t
    | ClangParameter of clang_parameter
    | CSharpParameter of Typ.t
    | ErlangParameter
  [@@deriving compare, equal]

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
  type mangled = string option [@@deriving compare, equal, yojson_of, sexp, hash]

  type kind =
    | CPPMethod of mangled
    | CPPConstructor of mangled
    | CPPDestructor of mangled
    | ObjCClassMethod
    | ObjCInstanceMethod
  [@@deriving compare, equal, yojson_of, sexp, hash]

  type t =
    { class_name: Typ.Name.t
    ; kind: kind
    ; method_name: string
    ; parameters: Parameter.clang_parameter list
    ; template_args: Typ.template_spec_info }
  [@@deriving compare, equal, yojson_of, sexp, hash]

  let make class_name method_name kind template_args parameters =
    {class_name; method_name; kind; template_args; parameters}


  let make_dealloc name = make name "dealloc" ObjCInstanceMethod Typ.NoTemplate []

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


  let pp verbosity fmt osig =
    let sep = if is_objc_method osig then "." else "::" in
    match verbosity with
    | Simple ->
        F.pp_print_string fmt osig.method_name
    | Non_verbose | NameOnly ->
        F.fprintf fmt "%s%s%s" (Typ.Name.name osig.class_name) sep osig.method_name
    | Verbose ->
        F.fprintf fmt "%s%s%s%a%a" (Typ.Name.name osig.class_name) sep osig.method_name
          Parameter.pp_parameters osig.parameters pp_verbose_kind osig.kind


  let remove_templates name =
    match String.lsplit2 ~on:'<' name with
    | Some (name_without_template, _template_part) ->
        name_without_template
    | None ->
        name


  let pp_without_templates fmt osig =
    F.fprintf fmt "%s::%s"
      (Typ.Name.name_without_templates osig.class_name)
      (remove_templates osig.method_name)


  let get_parameters osig = osig.parameters

  let replace_parameters new_parameters osig = {osig with parameters= new_parameters}

  module Normalizer = HashNormalizer.Make (struct
    type nonrec t = t [@@deriving equal]

    let hash = Hashtbl.hash

    let normalize t =
      let class_name = Typ.NameNormalizer.normalize t.class_name in
      let method_name = HashNormalizer.StringNormalizer.normalize t.method_name in
      let parameters =
        IList.map_changed ~equal:phys_equal ~f:Parameter.ClangParameterNormalizer.normalize
          t.parameters
      in
      if
        phys_equal class_name t.class_name
        && phys_equal method_name t.method_name
        && phys_equal parameters t.parameters
      then t
      else {class_name; kind= t.kind; method_name; parameters; template_args= t.template_args}
  end)
end

module C = struct
  (** Type of c procedure names. *)
  type t =
    { name: QualifiedCppName.t
    ; mangled: string option
    ; parameters: Parameter.clang_parameter list
    ; template_args: Typ.template_spec_info }
  [@@deriving compare, equal, yojson_of, sexp, hash]

  let c name ?mangled parameters template_args = {name; mangled; parameters; template_args}

  let from_string name =
    { name= QualifiedCppName.of_qual_string name
    ; mangled= None
    ; parameters= []
    ; template_args= NoTemplate }


  let pp verbosity fmt {name; mangled; parameters} =
    let plain = QualifiedCppName.to_qual_string name in
    match verbosity with
    | Simple ->
        F.fprintf fmt "%s()" plain
    | Non_verbose | NameOnly ->
        F.pp_print_string fmt plain
    | Verbose ->
        let pp_mangled fmt = function None -> () | Some s -> F.fprintf fmt "{%s}" s in
        F.fprintf fmt "%s%a%a" plain Parameter.pp_parameters parameters pp_mangled mangled


  let get_parameters c = c.parameters

  let replace_parameters new_parameters c = {c with parameters= new_parameters}

  (** NOTE: [std::_] is parsed as [C] proc name in Sil, rather than [ObjC_Cpp]. *)
  let is_std_function ~prefix {name} =
    match QualifiedCppName.to_rev_list name with
    | [fname; "std"] when String.is_prefix fname ~prefix ->
        true
    | _ ->
        false


  let is_make_shared c = is_std_function ~prefix:"make_shared" c

  let is_std_move c = is_std_function ~prefix:"move" c

  module Normalizer = HashNormalizer.Make (struct
    type nonrec t = t [@@deriving equal]

    let hash = Hashtbl.hash

    let normalize t =
      let name = QualifiedCppName.Normalizer.normalize t.name in
      let parameters =
        IList.map_changed ~equal:phys_equal ~f:Parameter.ClangParameterNormalizer.normalize
          t.parameters
      in
      if phys_equal name t.name && phys_equal parameters t.parameters then t
      else {name; mangled= t.mangled; parameters; template_args= t.template_args}
  end)
end

module Erlang = struct
  type t = {module_name: string; function_name: string; arity: int}
  [@@deriving compare, equal, yojson_of, sexp, hash]

  let pp_general arity_sep verbosity fmt {module_name; function_name; arity} =
    match verbosity with
    | Simple | Non_verbose ->
        F.fprintf fmt "%s%c%d" function_name arity_sep arity
    | Verbose ->
        F.fprintf fmt "%s:%s%c%d" module_name function_name arity_sep arity
    | NameOnly ->
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
  type block_type =
    | InOuterScope of {outer_scope: block_type; block_index: int}
    | SurroundingProc of {class_name: Typ.name option; name: string}
  [@@deriving compare, equal, yojson_of, sexp, hash]

  type t = {block_type: block_type; parameters: Parameter.clang_parameter list}
  [@@deriving compare, equal, yojson_of, sexp, hash]

  let make_surrounding class_name name parameters =
    {block_type= SurroundingProc {class_name; name}; parameters}


  let make_in_outer_scope outer_scope block_index parameters =
    {block_type= InOuterScope {outer_scope; block_index}; parameters}


  let pp_block_type fmt ~with_prefix_and_index =
    let prefix = if with_prefix_and_index then Config.anonymous_block_prefix else "^" in
    let pp_index fmt index =
      if with_prefix_and_index then F.fprintf fmt "%s%d" Config.anonymous_block_num_sep index
    in
    let rec aux fmt proc =
      match proc with
      | SurroundingProc {name} ->
          F.pp_print_string fmt name
      | InOuterScope {outer_scope; block_index} ->
          F.fprintf fmt "%s%a%a" prefix aux outer_scope pp_index block_index
    in
    aux fmt


  let pp verbosity fmt bsig =
    let pp_block = pp_block_type ~with_prefix_and_index:true in
    match verbosity with
    | Simple | NameOnly ->
        F.pp_print_string fmt "block"
    | Non_verbose ->
        pp_block fmt bsig.block_type
    | Verbose ->
        F.fprintf fmt "%a%a" pp_block bsig.block_type Parameter.pp_parameters bsig.parameters


  let get_parameters block = block.parameters

  let replace_parameters new_parameters block = {block with parameters= new_parameters}

  let get_class_type_name {block_type} =
    let rec get_class_type_name_aux = function
      | InOuterScope {outer_scope} ->
          get_class_type_name_aux outer_scope
      | SurroundingProc {class_name} ->
          class_name
    in
    get_class_type_name_aux block_type


  let get_class_name block = get_class_type_name block |> Option.map ~f:Typ.Name.name
end

module FunctionParameters = struct
  type t = FunPtr of C.t | Block of Block.t [@@deriving compare, equal, yojson_of, sexp, hash]

  let pp verbose f = function
    | FunPtr c ->
        C.pp verbose f c
    | Block block ->
        Block.pp verbose f block
end

module Hack = struct
  type t = {class_name: string option; function_name: string}
  [@@deriving compare, equal, yojson_of, sexp, hash]

  let get_class_type_name {class_name} =
    Option.map class_name ~f:(fun cn -> Typ.HackClass (HackClassName.make cn))


  let pp verbosity fmt t =
    match verbosity with
    | Simple | Non_verbose | NameOnly ->
        F.fprintf fmt "%s" t.function_name
    | Verbose -> (
      match t.class_name with
      | Some class_name ->
          F.fprintf fmt "%s.%s" class_name t.function_name
      | _ ->
          F.fprintf fmt "%s" t.function_name )
end

(** Type of procedure names. *)
type t =
  | Block of Block.t
  | C of C.t
  | CSharp of CSharp.t
  | Erlang of Erlang.t
  | Hack of Hack.t
  | Java of Java.t
  | Linters_dummy_method
  | ObjC_Cpp of ObjC_Cpp.t
  | WithAliasingParameters of t * Mangled.t list list
  | WithFunctionParameters of t * FunctionParameters.t list
[@@deriving compare, equal, yojson_of, sexp, hash]

let rec is_c = function
  | C _ ->
      true
  | WithAliasingParameters (base, _) | WithFunctionParameters (base, _) ->
      is_c base
  | _ ->
      false


let is_erlang_unsupported name =
  match name with
  | Erlang {module_name; _} ->
      String.equal module_name ErlangTypeName.unsupported
  | _ ->
      false


let is_erlang_call_unqualified name =
  match name with Erlang erlang_name -> Erlang.is_call_unqualified erlang_name | _ -> false


let is_erlang_call_qualified name =
  match name with Erlang erlang_name -> Erlang.is_call_qualified erlang_name | _ -> false


let is_erlang = function Erlang _ -> true | _ -> false

let rec compare_name x y =
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
  | C {name= name1}, C {name= name2} ->
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
  | Linters_dummy_method, Linters_dummy_method ->
      0
  | Linters_dummy_method, _ ->
      -1
  | _, Linters_dummy_method ->
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
  | ( (WithAliasingParameters (x, _) | WithFunctionParameters (x, _))
    , (WithAliasingParameters (y, _) | WithFunctionParameters (y, _)) ) ->
      compare_name x y


(** hash function for procname *)
let hash = Hashtbl.hash

let with_aliasing_parameters base aliases = WithAliasingParameters (base, aliases)

let with_function_parameters base functions = WithFunctionParameters (base, functions)

let rec base_of = function
  | WithAliasingParameters (base, _) | WithFunctionParameters (base, _) ->
      base_of base
  | base ->
      base


let is_std_move t = match base_of t with C c_pname -> C.is_std_move c_pname | _ -> false

let is_cpp_assignment_operator t =
  match base_of t with
  | ObjC_Cpp name when String.equal name.method_name "operator=" ->
      true
  | _ ->
      false


let is_destructor t =
  match base_of t with
  | ObjC_Cpp objc_cpp_pname ->
      ObjC_Cpp.is_destructor objc_cpp_pname
  | _ ->
      false


let is_csharp t = match base_of t with CSharp _ -> true | _ -> false

let is_hack t = match base_of t with Hack _ -> true | _ -> false

let is_java t = match base_of t with Java _ -> true | _ -> false

let as_java_exn ~explanation t =
  match base_of t with
  | Java java ->
      java
  | _ ->
      Logging.die InternalError "Expected Java procname: %s" explanation


(* TODO: deprecate this unfortunately named function and use is_clang instead *)
let is_c_method t = match base_of t with ObjC_Cpp _ -> true | _ -> false

let is_java_lift f t = match base_of t with Java java_pname -> f java_pname | _ -> false

let is_java_static_method = is_java_lift Java.is_static

let is_java_instance_method = is_java_lift Java.is_instance

let is_java_access_method = is_java_lift Java.is_access_method

let is_java_class_initializer = is_java_lift Java.is_class_initializer

let is_java_anonymous_inner_class_method = is_java_lift Java.is_anonymous_inner_class_method

let is_java_autogen_method = is_java_lift Java.is_autogen_method

let rec on_objc_helper ~f ~default = function
  | ObjC_Cpp objc_cpp_pname ->
      f objc_cpp_pname
  | WithAliasingParameters (base, _) | WithFunctionParameters (base, _) ->
      on_objc_helper ~f ~default base
  | Block _ | C _ | CSharp _ | Erlang _ | Hack _ | Java _ | Linters_dummy_method ->
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


let get_objc_class_name proc_name =
  get_objc_helper proc_name ~f:(fun objc_cpp_pname ->
      if ObjC_Cpp.is_objc_method objc_cpp_pname then Some (ObjC_Cpp.get_class_name objc_cpp_pname)
      else None )


let of_function_parameter = function
  | FunctionParameters.Block block ->
      Block block
  | FunctionParameters.FunPtr c ->
      C c


let to_function_parameter procname =
  match procname with
  | Block block ->
      FunctionParameters.Block block
  | C c ->
      FunctionParameters.FunPtr c
  | _ ->
      Logging.die InternalError "Only to be called with Objective-C block names or C function names"


let empty_block = Block (Block.make_surrounding None "" [])

(** Replace the class name component of a procedure name. In case of Java, replace package and class
    name. *)
let rec replace_class t (new_class : Typ.Name.t) =
  match t with
  | Java j ->
      Java {j with class_name= new_class}
  | CSharp cs ->
      CSharp {cs with class_name= new_class}
  | ObjC_Cpp osig ->
      ObjC_Cpp {osig with class_name= new_class}
  | WithAliasingParameters (base, aliases) ->
      WithAliasingParameters (replace_class base new_class, aliases)
  | WithFunctionParameters (base, functions) ->
      WithFunctionParameters (replace_class base new_class, functions)
  | C _ | Block _ | Erlang _ | Hack _ | Linters_dummy_method ->
      t


let get_class_type_name t =
  match base_of t with
  | Java java_pname ->
      Some (Java.get_class_type_name java_pname)
  | CSharp cs_pname ->
      Some (CSharp.get_class_type_name cs_pname)
  | ObjC_Cpp objc_pname ->
      Some (ObjC_Cpp.get_class_type_name objc_pname)
  | Block block ->
      Block.get_class_type_name block
  | Hack hack ->
      Hack.get_class_type_name hack
  | C _ | Erlang _ | WithAliasingParameters _ | WithFunctionParameters _ | Linters_dummy_method ->
      None


let get_class_name t =
  match base_of t with
  | Java java_pname ->
      Some (Java.get_class_name java_pname)
  | CSharp cs_pname ->
      Some (CSharp.get_class_name cs_pname)
  | ObjC_Cpp objc_pname ->
      Some (ObjC_Cpp.get_class_name objc_pname)
  | Block block ->
      Block.get_class_name block
  | _ ->
      None


let is_method_in_objc_protocol t =
  match t with ObjC_Cpp osig -> Typ.Name.is_objc_protocol osig.class_name | _ -> false


let rec objc_cpp_replace_method_name t (new_method_name : string) =
  match t with
  | ObjC_Cpp osig ->
      ObjC_Cpp {osig with method_name= new_method_name}
  | WithAliasingParameters (base, aliases) ->
      WithAliasingParameters (objc_cpp_replace_method_name base new_method_name, aliases)
  | WithFunctionParameters (base, functions) ->
      WithFunctionParameters (objc_cpp_replace_method_name base new_method_name, functions)
  | C _ | CSharp _ | Block _ | Erlang _ | Hack _ | Linters_dummy_method | Java _ ->
      t


(** Return the method/function of a procname. For Blocks, we don't display objc_block prefix or
    block index suffix. *)
let rec get_method = function
  | ObjC_Cpp name ->
      name.method_name
  | WithAliasingParameters (base, _) | WithFunctionParameters (base, _) ->
      get_method base
  | C {name} ->
      QualifiedCppName.to_qual_string name
  | Erlang name ->
      name.function_name
  | Hack name ->
      name.function_name
  | Block {block_type} ->
      F.asprintf "%a" (Block.pp_block_type ~with_prefix_and_index:false) block_type
  | Java j ->
      j.method_name
  | CSharp cs ->
      cs.method_name
  | Linters_dummy_method ->
      "Linters_dummy_method"


(** Return whether the procname is a block procname. *)
let rec is_objc_block = function
  | Block _ ->
      true
  | WithAliasingParameters (base, _) ->
      is_objc_block base
  | _ ->
      false


(** Return whether the procname is a specialized with functions procname. *)
let rec is_specialized_with_function_parameters = function
  | WithFunctionParameters _ ->
      true
  | WithAliasingParameters (base, _) ->
      is_specialized_with_function_parameters base
  | _ ->
      false


(** Return whether the procname is a cpp lambda procname. *)
let is_cpp_lambda t =
  match base_of t with
  | ObjC_Cpp cpp_pname when ObjC_Cpp.is_cpp_lambda cpp_pname ->
      true
  | _ ->
      false


(** Return the language of the procedure. *)
let rec get_language = function
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
  | Linters_dummy_method ->
      Language.Clang
  | WithAliasingParameters (base, _) | WithFunctionParameters (base, _) ->
      get_language base
  | Java _ ->
      Language.Java
  | CSharp _ ->
      Language.CIL


(** [is_constructor pname] returns true if [pname] is a constructor *)
let is_constructor t =
  match base_of t with
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
let is_infer_undefined pn =
  match base_of pn with
  | Java j ->
      let regexp = Str.regexp_string "com.facebook.infer.builtins.InferUndefined" in
      Str.string_match regexp (Java.get_class_name j) 0
  | _ ->
      (* TODO: add cases for obj-c, c, c++ *)
      false


let rec is_static = function
  | CSharp {kind= Static} | Java {kind= Static} | ObjC_Cpp {kind= ObjCClassMethod} ->
      Some true
  | CSharp {kind= Non_Static} | Java {kind= Non_Static} | ObjC_Cpp {kind= ObjCInstanceMethod} ->
      Some false
  | C _
  | Block _
  | Erlang _
  | Hack _
  | Linters_dummy_method
  | ObjC_Cpp {kind= CPPMethod _ | CPPConstructor _ | CPPDestructor _} ->
      None
  | WithAliasingParameters (base, _) ->
      is_static base
  | WithFunctionParameters (base, _) ->
      is_static base


let is_shared_ptr_observer =
  let shared_ptr_matcher =
    QualifiedCppName.Match.of_fuzzy_qual_names
      ["std::shared_ptr"; "std::__shared_ptr"; "std::__shared_ptr_access"]
  in
  let observer_methods = ["get"; "operator*"; "operator->"; "operator[]"; "operator_bool"] in
  let rec aux pname =
    match pname with
    | ObjC_Cpp {class_name= CppClass {name}; method_name} ->
        QualifiedCppName.Match.match_qualifiers shared_ptr_matcher name
        && List.mem observer_methods method_name ~equal:String.equal
    | WithAliasingParameters (pname, _) | WithFunctionParameters (pname, _) ->
        aux pname
    | _ ->
        false
  in
  fun pname -> aux pname


let get_global_name_of_initializer t =
  match base_of t with
  | C {name}
    when String.is_prefix ~prefix:Config.clang_initializer_prefix
           (QualifiedCppName.to_qual_string name) ->
      let name_str = QualifiedCppName.to_qual_string name in
      let prefix_len = String.length Config.clang_initializer_prefix in
      Some (String.sub name_str ~pos:prefix_len ~len:(String.length name_str - prefix_len))
  | _ ->
      None


let rec is_lambda_or_block = function
  | Block _ ->
      true
  | ObjC_Cpp {class_name} -> (
    match QualifiedCppName.extract_last (Typ.Name.unqualified_name class_name) with
    | Some (name, _) when String.is_prefix name ~prefix:"lambda" ->
        true
    | _ ->
        false )
  | WithAliasingParameters (base, _) | WithFunctionParameters (base, _) ->
      is_lambda_or_block base
  | _ ->
      false


let pp_with_aliasing_parameters verbose pp fmt base aliases =
  pp fmt base ;
  F.pp_print_string fmt "[" ;
  ( match verbose with
  | Non_verbose | Simple | NameOnly ->
      F.pp_print_string fmt "specialized with aliases"
  | Verbose ->
      let pp_alias fmt alias = Pp.seq ~sep:"=" Mangled.pp fmt alias in
      Pp.seq ~sep:"^" pp_alias fmt aliases ) ;
  F.pp_print_string fmt "]"


let pp_with_function_parameters verbose pp fmt base functions =
  pp fmt base ;
  F.pp_print_string fmt "[" ;
  ( match verbose with
  | Non_verbose | Simple | NameOnly ->
      let specialized_with =
        let open FunctionParameters in
        let contains_only_functions =
          List.for_all functions ~f:(function Block _ -> false | FunPtr _ -> true)
        in
        let contains_only_blocks =
          List.for_all functions ~f:(function Block _ -> true | FunPtr _ -> false)
        in
        if List.is_empty functions then
          Logging.(die InternalError) "Expected a non-empty list of function parameters"
        else if contains_only_functions then "functions"
        else if contains_only_blocks then "blocks"
        else "functions and blocks"
      in
      F.pp_print_string fmt ("specialized with " ^ specialized_with)
  | Verbose ->
      Pp.seq ~sep:"^" (FunctionParameters.pp verbose) fmt functions ) ;
  F.pp_print_string fmt "]"


(** Very verbose representation of an existing Procname.t *)
let rec pp_unique_id fmt = function
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
  | WithAliasingParameters (base, []) | WithFunctionParameters (base, []) ->
      pp_unique_id fmt base
  | WithAliasingParameters (base, aliases) ->
      pp_with_aliasing_parameters Verbose pp_unique_id fmt base aliases
  | WithFunctionParameters (base, functions) ->
      pp_with_function_parameters Verbose pp_unique_id fmt base functions
  | Linters_dummy_method ->
      F.pp_print_string fmt "Linters_dummy_method"


let to_unique_id proc_name = F.asprintf "%a" pp_unique_id proc_name

(** Convert a proc name to a string for the user to see *)
let rec pp_with_verbosity verbosity fmt = function
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
  | WithAliasingParameters (base, []) | WithFunctionParameters (base, []) ->
      pp_with_verbosity verbosity fmt base
  | WithAliasingParameters (base, aliases) ->
      pp_with_aliasing_parameters verbosity (pp_with_verbosity verbosity) fmt base aliases
  | WithFunctionParameters (base, (_ :: _ as functions)) ->
      pp_with_function_parameters verbosity (pp_with_verbosity verbosity) fmt base functions
  | Linters_dummy_method ->
      pp_unique_id fmt Linters_dummy_method


let pp = pp_with_verbosity Non_verbose

let pp_verbose = pp_with_verbosity Verbose

let pp_without_templates fmt = function
  | ObjC_Cpp osig when not (ObjC_Cpp.is_objc_method osig) ->
      ObjC_Cpp.pp_without_templates fmt osig
  | other ->
      (* For other languages, we use the formaters defined in pp *)
      pp fmt other


let to_string proc_name = F.asprintf "%a" pp proc_name

let get_block_type proc =
  match base_of proc with
  | Block {block_type} ->
      block_type
  | _ ->
      Block.SurroundingProc {class_name= get_class_type_name proc; name= to_string proc}


let rec pp_name_only fmt = function
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
  | WithAliasingParameters (base, _) | WithFunctionParameters (base, _) ->
      pp_name_only fmt base
  | Linters_dummy_method ->
      pp_unique_id fmt Linters_dummy_method


let patterns_match patterns proc_name =
  let s = F.asprintf "%a" pp_name_only proc_name in
  List.exists patterns ~f:(fun pattern -> Re.Str.string_match pattern s 0)


(** Convenient representation of a procname for external tools (e.g. eclipse plugin) *)
let rec pp_simplified_string ?(withclass = false) fmt = function
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
  | WithAliasingParameters (base, _) | WithFunctionParameters (base, _) ->
      pp_simplified_string fmt base
  | Linters_dummy_method ->
      pp_unique_id fmt Linters_dummy_method


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
      let name = F.asprintf "%a" (Block.pp Non_verbose) bsig in
      List.hd_exn (String.split name ~on:':')
  | _ ->
      (* Other cases for C and C++ method names *)
      F.asprintf "%a" (pp_simplified_string ~withclass:true) proc_name


let rec get_parameters procname =
  let clang_param_to_param clang_params =
    List.map ~f:(fun par -> Parameter.ClangParameter par) clang_params
  in
  match procname with
  | Java j ->
      List.map ~f:(fun par -> Parameter.JavaParameter par) (Java.get_parameters j)
  | CSharp cs ->
      List.map ~f:(fun par -> Parameter.CSharpParameter par) (CSharp.get_parameters cs)
  | C osig ->
      clang_param_to_param (C.get_parameters osig)
  | Erlang e ->
      List.init e.arity ~f:(fun _ -> Parameter.ErlangParameter)
  | Hack _ ->
      (* TODO(arr): we don't know yet how the parameters of Hack methods will be represented. Will refine later. *)
      []
  | ObjC_Cpp osig ->
      clang_param_to_param (ObjC_Cpp.get_parameters osig)
  | Block bsig ->
      clang_param_to_param (Block.get_parameters bsig)
  | WithAliasingParameters (base, _) | WithFunctionParameters (base, _) ->
      get_parameters base
  | Linters_dummy_method ->
      []


let rec replace_parameters new_parameters procname =
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
  | C osig ->
      C (C.replace_parameters (params_to_clang_params new_parameters) osig)
  | Erlang e ->
      Erlang (Erlang.set_arity (params_to_erlang_arity new_parameters) e)
  | Hack _ ->
      procname
  | ObjC_Cpp osig ->
      ObjC_Cpp (ObjC_Cpp.replace_parameters (params_to_clang_params new_parameters) osig)
  | Block bsig ->
      Block (Block.replace_parameters (params_to_clang_params new_parameters) bsig)
  | WithAliasingParameters (base, aliases) ->
      WithAliasingParameters (replace_parameters new_parameters base, aliases)
  | WithFunctionParameters (base, functions) ->
      WithFunctionParameters (replace_parameters new_parameters base, functions)
  | Linters_dummy_method ->
      procname


let parameter_of_name procname class_name =
  match base_of procname with
  | Java _ ->
      Parameter.JavaParameter Typ.(mk_ptr (mk_struct class_name))
  | CSharp _ ->
      Parameter.CSharpParameter Typ.(mk_ptr (mk_struct class_name))
  | _ ->
      Parameter.ClangParameter (Parameter.clang_param_of_name class_name)


let describe f pn =
  let name = hashable_name pn in
  match String.lsplit2 ~on:'<' name with
  | Some (name_without_template, _template_part) ->
      F.pp_print_string f name_without_template
  | None ->
      F.pp_print_string f name


let make_java ~class_name ~return_type ~method_name ~parameters ~kind =
  Java (Java.make ~class_name ~return_type ~method_name ~parameters ~kind ())


let make_csharp ~class_name ~return_type ~method_name ~parameters ~kind =
  CSharp (CSharp.make ~class_name ~return_type ~method_name ~parameters ~kind ())


let make_erlang ~module_name ~function_name ~arity = Erlang {module_name; function_name; arity}

let make_hack ~class_name ~function_name = Hack {class_name; function_name}

let make_objc_dealloc name = ObjC_Cpp (ObjC_Cpp.make_dealloc name)

let make_objc_copyWithZone ~is_mutable name = ObjC_Cpp (ObjC_Cpp.make_copyWithZone ~is_mutable name)

let erlang_call_unqualified ~arity = Erlang (Erlang.call_unqualified arity)

let erlang_call_qualified ~arity = Erlang (Erlang.call_qualified arity)

module Hashable = struct
  type nonrec t = t [@@deriving compare, equal]

  let hash = hash

  let sexp_of_t t = Sexp.of_string (to_string t)
end

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
  match base_of pname with
  | C {name} ->
      name
  | ObjC_Cpp objc_cpp ->
      ObjC_Cpp.get_class_qualifiers objc_cpp
      |> QualifiedCppName.append_qualifier ~qual:objc_cpp.method_name
  | _ ->
      QualifiedCppName.empty


let to_filename pname =
  (* filenames for clang procs are REVERSED qualifiers with '#' as separator *)
  let pp_rev_qualified fmt pname =
    let rev_qualifiers = get_qualifiers pname |> QualifiedCppName.to_rev_list in
    Pp.seq ~sep:"#" F.pp_print_string fmt rev_qualifiers
  in
  let proc_id =
    match pname with
    | C {parameters; mangled} ->
        let pp_mangled fmt = function None -> () | Some mangled -> F.fprintf fmt "#%s" mangled in
        F.asprintf "%a%a%a" pp_rev_qualified pname Parameter.pp_parameters parameters pp_mangled
          mangled
    | ObjC_Cpp objc_cpp ->
        F.asprintf "%a%a#%a" pp_rev_qualified pname Parameter.pp_parameters objc_cpp.parameters
          ObjC_Cpp.pp_verbose_kind objc_cpp.kind
    | _ ->
        F.asprintf "%a" pp_unique_id pname
  in
  DB.append_crc_cutoff proc_id |> fst


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

module Normalizer = HashNormalizer.Make (struct
  type nonrec t = t [@@deriving equal]

  let hash = hash

  let normalize t =
    match t with
    | Java java_pname ->
        let java_pname' = Java.Normalizer.normalize java_pname in
        if phys_equal java_pname java_pname' then t else Java java_pname'
    | C c ->
        let c' = C.Normalizer.normalize c in
        if phys_equal c c' then t else C c'
    | ObjC_Cpp objc_cpp ->
        let objc_cpp' = ObjC_Cpp.Normalizer.normalize objc_cpp in
        if phys_equal objc_cpp objc_cpp' then t else ObjC_Cpp objc_cpp'
    | Linters_dummy_method | WithAliasingParameters _ | WithFunctionParameters _ ->
        (* these kinds should not appear inside a type environment *)
        t
    | Block _ | CSharp _ | Erlang _ | Hack _ ->
        (* TODO *)
        t
end)
