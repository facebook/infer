(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Hashtbl = Caml.Hashtbl
module F = Format

(** Level of verbosity of some to_string functions. *)
type detail_level = Verbose | Non_verbose | Simple [@@deriving compare, equal]

let is_verbose v = match v with Verbose -> true | _ -> false

module Java = struct
  type kind =
    | Non_Static
        (** in Java, procedures called with invokevirtual, invokespecial, and invokeinterface *)
    | Static  (** in Java, procedures called with invokestatic *)
  [@@deriving compare]

  (* TODO: use Mangled.t here *)
  type java_type = Typ.Name.Java.Split.t [@@deriving compare, equal]

  let java_void = Typ.Name.Java.Split.make "void"

  (** Type of java procedure names. *)
  type t =
    { method_name: string
    ; parameters: java_type list
    ; class_name: Typ.Name.t
    ; return_type: java_type option (* option because constructors have no return type *)
    ; kind: kind }
  [@@deriving compare]

  let make class_name return_type method_name parameters kind =
    {class_name; return_type; method_name; parameters; kind}


  let pp_type_verbosity verbosity fmt java_type =
    Typ.Name.Java.Split.pp_type_verbosity ~verbose:(is_verbose verbosity) fmt java_type


  (** Given a list of types, it creates a unique string of types separated by commas *)
  let rec pp_param_list verbosity fmt inputList =
    match inputList with
    | [] ->
        ()
    | [head] ->
        pp_type_verbosity verbosity fmt head
    | head :: rest ->
        pp_type_verbosity verbosity fmt head ;
        F.pp_print_string fmt "," ;
        pp_param_list verbosity fmt rest


  let java_type_of_name class_name = Typ.Name.Java.Split.of_string (Typ.Name.name class_name)

  (** It is the same as java_type_to_string_verbosity, but Java return types are optional because of
      constructors without type *)
  let pp_return_type verbosity fmt j =
    match j.return_type with None -> () | Some typ -> pp_type_verbosity verbosity fmt typ


  let get_class_name j = Typ.Name.name j.class_name

  let get_class_type_name j = j.class_name

  let get_simple_class_name j = Typ.Name.Java.Split.(j |> get_class_name |> of_string |> type_name)

  let get_package j = Typ.Name.Java.Split.(j |> get_class_name |> of_string |> package)

  let get_method j = j.method_name

  let replace_method_name method_name j = {j with method_name}

  let replace_parameters parameters j = {j with parameters}

  let replace_return_type ret_type j = {j with return_type= Some ret_type}

  let get_parameters j = j.parameters

  (** Prints a string of a java procname with the given level of verbosity *)
  let pp ?(withclass = false) verbosity fmt j =
    match verbosity with
    | Verbose | Non_verbose ->
        (* if verbose, then package.class.method(params): rtype,
           else rtype package.class.method(params)
           verbose is used for example to create unique filenames, non_verbose to create reports *)
        let pp_class_name verbosity fmt j =
          pp_type_verbosity verbosity fmt (Typ.Name.Java.split_typename j.class_name)
        in
        let separator =
          match (j.return_type, verbosity) with None, _ -> "" | Some _, Verbose -> ":" | _ -> " "
        in
        if not (equal_detail_level verbosity Verbose) then
          F.fprintf fmt "%a%s" (pp_return_type verbosity) j separator ;
        F.fprintf fmt "%a.%s(%a)" (pp_class_name verbosity) j j.method_name
          (pp_param_list verbosity) j.parameters ;
        if equal_detail_level verbosity Verbose then
          F.fprintf fmt "%s%a" separator (pp_return_type verbosity) j
    | Simple ->
        (* methodname(...) or without ... if there are no parameters *)
        let pp_class_prefix ~withclass verbosity fmt j =
          if withclass then
            F.fprintf fmt "%a." (pp_type_verbosity verbosity)
              (Typ.Name.Java.split_typename j.class_name)
        in
        let params = match j.parameters with [] -> "" | _ -> "..." in
        let pp_method_name ~withclass verbosity fmt j =
          if String.equal j.method_name "<init>" then
            F.pp_print_string fmt (get_simple_class_name j)
          else F.fprintf fmt "%a%s" (pp_class_prefix ~withclass verbosity) j j.method_name
        in
        F.fprintf fmt "%a(%s)" (pp_method_name ~withclass verbosity) j params


  let get_return_typ pname_java =
    let rec java_from_string = function
      | "" | "void" ->
          Typ.void
      | "int" ->
          Typ.int
      | "byte" ->
          Typ.java_byte
      | "short" ->
          Typ.java_short
      | "boolean" ->
          Typ.boolean
      | "char" ->
          Typ.char
      | "long" ->
          Typ.long
      | "float" ->
          Typ.float
      | "double" ->
          Typ.double
      | typ_str when String.contains typ_str '[' ->
          let stripped_typ = String.sub typ_str ~pos:0 ~len:(String.length typ_str - 2) in
          Typ.mk (Tptr (Typ.mk_array (java_from_string stripped_typ), Pk_pointer))
      | typ_str ->
          Typ.mk (Tstruct (Typ.Name.Java.from_string typ_str))
    in
    let typ = java_from_string (F.asprintf "%a" (pp_return_type Verbose) pname_java) in
    match typ.desc with Tstruct _ -> Typ.mk (Tptr (typ, Pk_pointer)) | _ -> typ


  let is_close {method_name} = String.equal method_name "close"

  let constructor_method_name = "<init>"

  let class_initializer_method_name = "<clinit>"

  let is_class_initializer {method_name} = String.equal method_name class_initializer_method_name

  let get_class_initializer class_name =
    { method_name= class_initializer_method_name
    ; parameters= []
    ; class_name
    ; return_type= Some java_void
    ; kind= Static }


  let is_constructor {method_name} = String.equal method_name constructor_method_name

  let is_anonymous_inner_class_constructor {class_name} =
    Typ.Name.Java.is_anonymous_inner_class_name class_name


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

  (** Check if the proc name has the type of a java vararg. Note: currently only checks that the
      last argument has type Object[]. *)
  let is_vararg {parameters} =
    (* FIXME this looks wrong due to the dot in the type name *)
    List.last parameters
    |> Option.exists ~f:(fun java_type ->
           String.equal "java.lang.Object[]" (Typ.Name.Java.Split.type_name java_type) )


  let is_external java_pname =
    let package = get_package java_pname in
    Option.exists ~f:Config.java_package_is_external package
end

module Parameter = struct
  (** Type for parameters in clang procnames, [Some name] means the parameter is of type pointer to
      struct, with [name] being the name of the struct, [None] means the parameter is of some other
      type. *)
  type clang_parameter = Typ.Name.t option [@@deriving compare, equal]

  (** Type for parameters in procnames, for java and clang. *)
  type t = JavaParameter of Java.java_type | ClangParameter of clang_parameter
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
  type kind =
    | CPPMethod of {mangled: string option}
    | CPPConstructor of {mangled: string option; is_constexpr: bool}
    | CPPDestructor of {mangled: string option}
    | ObjCClassMethod
    | ObjCInstanceMethod
    | ObjCInternalMethod
  [@@deriving compare]

  type t =
    { class_name: Typ.Name.t
    ; kind: kind
    ; method_name: string
    ; parameters: Parameter.clang_parameter list
    ; template_args: Typ.template_spec_info }
  [@@deriving compare]

  let make class_name method_name kind template_args parameters =
    {class_name; method_name; kind; template_args; parameters}


  let get_class_name objc_cpp = Typ.Name.name objc_cpp.class_name

  let get_class_type_name objc_cpp = objc_cpp.class_name

  let get_class_qualifiers objc_cpp = Typ.Name.qual_name objc_cpp.class_name

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


  let is_inner_destructor ({method_name} as pname) =
    is_destructor pname && String.is_prefix ~prefix:Config.clang_inner_destructor_prefix method_name


  let is_constexpr = function {kind= CPPConstructor {is_constexpr= true}} -> true | _ -> false

  let is_cpp_lambda {method_name} = String.is_substring ~substring:"operator()" method_name

  let pp_verbose_kind fmt = function
    | CPPMethod {mangled} | CPPDestructor {mangled} ->
        F.fprintf fmt "(%s)" (Option.value ~default:"" mangled)
    | CPPConstructor {mangled; is_constexpr} ->
        F.fprintf fmt "{%s%s}"
          (Option.value ~default:"" mangled)
          (if is_constexpr then "|constexpr" else "")
    | ObjCClassMethod ->
        F.pp_print_string fmt "class"
    | ObjCInstanceMethod ->
        F.pp_print_string fmt "instance"
    | ObjCInternalMethod ->
        F.pp_print_string fmt "internal"


  let pp verbosity fmt osig =
    match verbosity with
    | Simple ->
        F.pp_print_string fmt osig.method_name
    | Non_verbose ->
        F.fprintf fmt "%s::%s" (Typ.Name.name osig.class_name) osig.method_name
    | Verbose ->
        F.fprintf fmt "%s::%s%a%a" (Typ.Name.name osig.class_name) osig.method_name
          Parameter.pp_parameters osig.parameters pp_verbose_kind osig.kind


  let get_parameters osig = osig.parameters

  let replace_parameters new_parameters osig = {osig with parameters= new_parameters}
end

module C = struct
  (** Type of c procedure names. *)
  type t =
    { name: QualifiedCppName.t
    ; mangled: string option
    ; parameters: Parameter.clang_parameter list
    ; template_args: Typ.template_spec_info }
  [@@deriving compare]

  let c name mangled parameters template_args =
    {name; mangled= Some mangled; parameters; template_args}


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
    | Non_verbose ->
        F.pp_print_string fmt plain
    | Verbose ->
        let pp_mangled fmt = function None -> () | Some s -> F.fprintf fmt "{%s}" s in
        F.fprintf fmt "%s%a%a" plain Parameter.pp_parameters parameters pp_mangled mangled


  let get_parameters c = c.parameters

  let replace_parameters new_parameters c = {c with parameters= new_parameters}

  (** NOTE: [std::make_shared] is parsed as [C] proc name in Sil, rather than [ObjC_Cpp]. *)
  let is_make_shared {name} =
    match QualifiedCppName.to_rev_list name with
    | [make_shared; "std"] when String.is_prefix make_shared ~prefix:"make_shared" ->
        true
    | _ ->
        false
end

module Block = struct
  (** Type of Objective C block names. *)
  type block_name = string [@@deriving compare]

  type t = {name: block_name; parameters: Parameter.clang_parameter list} [@@deriving compare]

  let make name parameters = {name; parameters}

  let pp verbosity fmt bsig =
    match verbosity with
    | Simple ->
        F.pp_print_string fmt "block"
    | Non_verbose ->
        F.pp_print_string fmt bsig.name
    | Verbose ->
        F.fprintf fmt "%s%a" bsig.name Parameter.pp_parameters bsig.parameters


  let get_parameters block = block.parameters

  let replace_parameters new_parameters block = {block with parameters= new_parameters}
end

(** Type of procedure names. *)
type t =
  | Java of Java.t
  | C of C.t
  | Linters_dummy_method
  | Block of Block.t
  | ObjC_Cpp of ObjC_Cpp.t
  | WithBlockParameters of t * Block.block_name list
[@@deriving compare]

let equal = [%compare.equal: t]

(** hash function for procname *)
let hash = Hashtbl.hash

let with_block_parameters base blocks = WithBlockParameters (base, blocks)

let is_java = function Java _ -> true | _ -> false

(* TODO: deprecate this unfortunately named function and use is_clang instead *)
let is_c_method = function ObjC_Cpp _ -> true | _ -> false

let is_c_function = function C _ -> true | _ -> false

let is_clang = function ObjC_Cpp name -> ObjC_Cpp.is_objc_method name | name -> is_c_function name

let is_java_lift f = function Java java_pname -> f java_pname | _ -> false

let is_java_access_method = is_java_lift Java.is_access_method

let is_java_class_initializer = is_java_lift Java.is_class_initializer

let is_objc_method procname =
  match procname with ObjC_Cpp name -> ObjC_Cpp.is_objc_method name | _ -> false


let block_name_of_procname procname =
  match procname with
  | Block block ->
      block.name
  | _ ->
      Logging.die InternalError "Only to be called with Objective-C block names"


let empty_block = Block {name= ""; parameters= []}

(** Replace the class name component of a procedure name. In case of Java, replace package and class
    name. *)
let rec replace_class t (new_class : Typ.Name.t) =
  match t with
  | Java j ->
      Java {j with class_name= new_class}
  | ObjC_Cpp osig ->
      ObjC_Cpp {osig with class_name= new_class}
  | WithBlockParameters (base, blocks) ->
      WithBlockParameters (replace_class base new_class, blocks)
  | C _ | Block _ | Linters_dummy_method ->
      t


let get_class_type_name = function
  | Java java_pname ->
      Some (Java.get_class_type_name java_pname)
  | ObjC_Cpp objc_pname ->
      Some (ObjC_Cpp.get_class_type_name objc_pname)
  | _ ->
      None


let get_class_name = function
  | Java java_pname ->
      Some (Java.get_class_name java_pname)
  | ObjC_Cpp objc_pname ->
      Some (ObjC_Cpp.get_class_name objc_pname)
  | _ ->
      None


let is_method_in_objc_protocol t =
  match t with ObjC_Cpp osig -> Typ.Name.is_objc_protocol osig.class_name | _ -> false


let rec objc_cpp_replace_method_name t (new_method_name : string) =
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
  | Block {name} ->
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
      String.equal js.method_name Java.constructor_method_name
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
      let regexp = Str.regexp_string "com.facebook.infer.builtins.InferUndefined" in
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


(** Very verbose representation of an existing Procname.t *)
let rec pp_unique_id fmt = function
  | Java j ->
      Java.pp Verbose fmt j
  | C osig ->
      C.pp Verbose fmt osig
  | ObjC_Cpp osig ->
      ObjC_Cpp.pp Verbose fmt osig
  | Block bsig ->
      Block.pp Verbose fmt bsig
  | WithBlockParameters (base, []) ->
      pp_unique_id fmt base
  | WithBlockParameters (base, (_ :: _ as blocks)) ->
      pp_unique_id fmt base ;
      F.pp_print_string fmt "_" ;
      Pp.seq ~sep:"_" F.pp_print_string fmt blocks
  | Linters_dummy_method ->
      F.pp_print_string fmt "Linters_dummy_method"


let to_unique_id proc_name = F.asprintf "%a" pp_unique_id proc_name

(** Convert a proc name to a string for the user to see *)
let rec pp fmt = function
  | Java j ->
      Java.pp Non_verbose fmt j
  | C osig ->
      C.pp Non_verbose fmt osig
  | ObjC_Cpp osig ->
      ObjC_Cpp.pp Non_verbose fmt osig
  | Block bsig ->
      Block.pp Non_verbose fmt bsig
  | WithBlockParameters (base, []) ->
      pp fmt base
  | WithBlockParameters (base, (_ :: _ as blocks)) ->
      pp fmt base ;
      F.pp_print_string fmt "_" ;
      Pp.seq ~sep:"_" F.pp_print_string fmt blocks
  | Linters_dummy_method ->
      pp_unique_id fmt Linters_dummy_method


let to_string proc_name = F.asprintf "%a" pp proc_name

(** Convenient representation of a procname for external tools (e.g. eclipse plugin) *)
let rec pp_simplified_string ?(withclass = false) fmt = function
  | Java j ->
      Java.pp ~withclass Simple fmt j
  | C osig ->
      C.pp Simple fmt osig
  | ObjC_Cpp osig ->
      ObjC_Cpp.pp (if withclass then Non_verbose else Simple) fmt osig
  | Block bsig ->
      Block.pp Simple fmt bsig
  | WithBlockParameters (base, _) ->
      pp_simplified_string fmt base
  | Linters_dummy_method ->
      pp_unique_id fmt Linters_dummy_method


let to_simplified_string ?withclass proc_name =
  F.asprintf "%a" (pp_simplified_string ?withclass) proc_name


let from_string_c_fun func = C (C.from_string func)

let java_inner_class_prefix_regex = Str.regexp "\\$[0-9]+"

let hashable_name proc_name =
  match proc_name with
  | Java pname -> (
      (* Strip autogenerated anonymous inner class numbers in order to keep the bug hash
         invariant when introducing new anonymous classes *)
      let name = F.asprintf "%a" (Java.pp ~withclass:true Simple) pname in
      match Str.search_forward java_inner_class_prefix_regex name 0 with
      | _ ->
          Str.global_replace java_inner_class_prefix_regex "$_" name
      | exception Caml.Not_found ->
          name )
  | ObjC_Cpp m when ObjC_Cpp.is_objc_method m ->
      (* In Objective C, the list of parameters is part of the method name. To prevent the bug
         hash to change when a parameter is introduced or removed, only the part of the name
         before the first colon is used for the bug hash *)
      let name = F.asprintf "%a" (pp_simplified_string ~withclass:true) proc_name in
      List.hd_exn (String.split_on_chars name ~on:[':'])
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
  | C osig ->
      clang_param_to_param (C.get_parameters osig)
  | ObjC_Cpp osig ->
      clang_param_to_param (ObjC_Cpp.get_parameters osig)
  | Block bsig ->
      clang_param_to_param (Block.get_parameters bsig)
  | WithBlockParameters (base, _) ->
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
  match procname with
  | Java j ->
      Java (Java.replace_parameters (params_to_java_params new_parameters) j)
  | C osig ->
      C (C.replace_parameters (params_to_clang_params new_parameters) osig)
  | ObjC_Cpp osig ->
      ObjC_Cpp (ObjC_Cpp.replace_parameters (params_to_clang_params new_parameters) osig)
  | Block bsig ->
      Block (Block.replace_parameters (params_to_clang_params new_parameters) bsig)
  | WithBlockParameters (base, blocks) ->
      WithBlockParameters (replace_parameters new_parameters base, blocks)
  | Linters_dummy_method ->
      procname


let parameter_of_name procname class_name =
  match procname with
  | Java _ ->
      Parameter.JavaParameter (Java.java_type_of_name class_name)
  | _ ->
      Parameter.ClangParameter (Parameter.clang_param_of_name class_name)


let describe f pn =
  let name = hashable_name pn in
  match String.lsplit2 ~on:'<' name with
  | Some (name_without_template, _template_part) ->
      F.pp_print_string f name_without_template
  | None ->
      F.pp_print_string f name


module Hashable = struct
  type nonrec t = t

  let equal = equal

  let hash = hash
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
  Escape.escape_filename @@ DB.append_crc_cutoff proc_id


module SQLite = struct
  module T = struct
    type nonrec t = t

    let compare = compare

    let hash = hash

    let sexp_of_t p = Sexp.Atom (F.asprintf "%a" pp p)
  end

  module Serializer = SqliteUtils.MarshalledDataForComparison (T)

  let pname_to_key = Base.Hashtbl.create (module T)

  let serialize pname =
    let default () = Serializer.serialize pname in
    Base.Hashtbl.find_or_add pname_to_key pname ~default


  let deserialize = Serializer.deserialize

  let clear_cache () = Base.Hashtbl.clear pname_to_key
end

module SQLiteList = SqliteUtils.MarshalledDataNOTForComparison (struct
  type nonrec t = t list
end)
