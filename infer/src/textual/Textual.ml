(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module Hashtbl = Caml.Hashtbl

module Lang = struct
  type t = Java | Hack | Python [@@deriving equal]

  let of_string s =
    match String.lowercase s with
    | "java" ->
        Some Java
    | "hack" ->
        Some Hack
    | "python" ->
        Some Python
    | _ ->
        None


  let to_string = function Java -> "java" | Hack -> "hack" | Python -> "python"
end

module Location = struct
  module T = struct
    type t = Known of {line: int; col: int} | Unknown [@@deriving compare]
  end

  include T

  let known ~line ~col = Known {line; col}

  let pp fmt = function
    | Known {line; col} ->
        F.fprintf fmt "line %d, column %d" line col
    | Unknown ->
        F.fprintf fmt "<unknown location>"


  let pp_line fmt = function
    | Known {line} ->
        F.fprintf fmt "line %d" line
    | Unknown ->
        F.fprintf fmt "<unknown line>"


  module Set = Caml.Set.Make (T)
end

module SourceFile = struct
  type t = {file: SourceFile.t; line_map: LineMap.t option}

  let create ?line_map filename = {file= SourceFile.create filename; line_map}

  let line_map {line_map} = line_map

  let file {file} = file

  let pp fmt {file} = SourceFile.pp fmt file
end

type transform_error = {loc: Location.t; msg: string Lazy.t}

let pp_transform_error sourcefile fmt {loc; msg} =
  F.fprintf fmt "%a, %a: transformation error: %s" SourceFile.pp sourcefile Location.pp loc
    (Lazy.force msg)


exception TextualTransformError of transform_error list

module type NAME = sig
  type t = {value: string; loc: Location.t [@compare.ignore]} [@@deriving compare, equal, hash]

  val of_java_name : string -> t

  val pp : F.formatter -> t -> unit

  val is_hack_init : t -> bool

  module Hashtbl : Hashtbl.S with type key = t

  module HashSet : HashSet.S with type elt = t

  module Map : Caml.Map.S with type key = t

  module Set : Caml.Set.S with type elt = t
end

module Name : NAME = struct
  module T = struct
    type t = {value: string; loc: Location.t [@compare.ignore] [@equal.ignore] [@hash.ignore]}
    [@@deriving compare, equal, hash]
  end

  include T

  let replace_dot_with_2colons str = String.substr_replace_all str ~pattern:"." ~with_:"::"

  let of_java_name str = {value= replace_dot_with_2colons str; loc= Location.Unknown}

  let pp fmt name = F.pp_print_string fmt name.value

  let is_hack_init {value} = String.equal value "_86pinit" || String.equal value "_86sinit"

  module Hashtbl = Hashtbl.Make (T)
  module HashSet = HashSet.Make (T)
  module Map = Caml.Map.Make (T)
  module Set = Caml.Set.Make (T)
end

module ProcName : NAME = Name

module FieldName : NAME = Name

let builtin_allocate = "__sil_allocate"

let builtin_allocate_array = "__sil_allocate_array"

let builtin_lazy_class_initialize = "__sil_lazy_class_initialize"

let builtin_generics_constructor = "__sil_generics"

let builtin_cast = "__sil_cast"

let builtin_get_lazy_class = "__sil_get_lazy_class"

let builtin_instanceof = "__sil_instanceof"

module TypeName : sig
  include NAME

  val hack_builtin : t

  val hack_generics : t

  val wildcard : t
end = struct
  include Name

  let wildcard = {value= "?"; loc= Location.Unknown}

  let hack_builtin = {value= "$builtins"; loc= Location.Unknown}

  let hack_generics = {value= "HackGenerics"; loc= Location.Unknown}
end

module QualifiedProcName = struct
  type enclosing_class = TopLevel | Enclosing of TypeName.t [@@deriving equal, hash, compare]

  type t = {enclosing_class: enclosing_class; name: ProcName.t} [@@deriving compare, equal, hash]
  (* procedure name [name] is attached to the name space [enclosing_class] *)

  let pp_enclosing_class fmt = function
    | TopLevel ->
        ()
    | Enclosing tname ->
        F.fprintf fmt "%a." TypeName.pp tname


  let pp fmt {enclosing_class; name} =
    F.fprintf fmt "%a%a" pp_enclosing_class enclosing_class ProcName.pp name


  let contains_wildcard {enclosing_class} =
    match enclosing_class with
    | Enclosing class_name ->
        TypeName.equal class_name TypeName.wildcard
    | TopLevel ->
        false


  let name {name} = name

  let is_hack_init {name} = ProcName.is_hack_init name

  module Hashtbl = Hashtbl.Make (struct
    type nonrec t = t [@@deriving equal, hash]
  end)
end

type qualified_fieldname = {enclosing_class: TypeName.t; name: FieldName.t}
(* field name [name] must be declared in type [enclosing_class] *)

let pp_qualified_fieldname fmt ({enclosing_class; name} : qualified_fieldname) =
  F.fprintf fmt "%a%a" TypeName.pp enclosing_class FieldName.pp name


module VarName : sig
  include NAME

  val is_hack_reified_generics_param : t -> bool
end = struct
  include Name

  let is_hack_reified_generics_param {value} = String.equal value "$0ReifiedGenerics"
end

module NodeName : NAME = Name

module Attr = struct
  type t = {name: string; values: string list; loc: Location.t}

  let name {name} = name

  let values {values} = values

  let source_language = "source_language"

  let mk_source_language value =
    {name= source_language; values= [Lang.to_string value]; loc= Location.Unknown}


  let mk_static = {name= "static"; values= []; loc= Location.Unknown}

  let mk_final = {name= "final"; values= []; loc= Location.Unknown}

  let mk_trait = {name= "kind"; values= ["trait"]; loc= Location.Unknown}

  let is_async {name; values} = String.equal name "async" && List.is_empty values

  let is_hack_wrapper {name; values} = String.equal name "wrapper" && List.is_empty values

  let is_abstract {name; values} = String.equal name "abstract" && List.is_empty values

  let is_alias {name; values} =
    String.equal name "kind" && List.equal String.equal values ["typedef"]


  let is_const {name; values} = String.equal name "constant" && List.is_empty values

  let is_curry {name; values} = String.equal name "curry" && List.is_empty values

  let is_final {name; values} = String.equal name "final" && List.is_empty values

  let is_notnull {name; values} = String.equal name "notnull" && List.is_empty values

  let is_interface {name; values} =
    String.equal name "kind" && List.equal String.equal values ["interface"]


  let is_trait {name; values} = String.equal name "kind" && List.equal String.equal values ["trait"]

  let is_static {name; values} = String.equal name "static" && List.is_empty values

  let is_variadic {name; values} = String.equal name "variadic" && List.is_empty values

  let pp fmt {name; values} =
    if List.is_empty values then F.fprintf fmt ".%s" name
    else F.fprintf fmt ".%s = \"%a\"" name (Pp.comma_seq F.pp_print_string) values


  let pp_with_loc fmt t = F.fprintf fmt "%a: %a" Location.pp t.loc pp t
end

module Typ = struct
  type t = Int | Float | Null | Void | Ptr of t | Struct of TypeName.t | Array of t
  [@@deriving equal, hash]

  let rec pp fmt = function
    | Int ->
        F.pp_print_string fmt "int"
    | Float ->
        F.pp_print_string fmt "float"
    | Null ->
        F.pp_print_string fmt "null"
    | Void ->
        F.pp_print_string fmt "void"
    | Ptr typ ->
        F.pp_print_char fmt '*' ;
        pp fmt typ
    | Struct name ->
        TypeName.pp fmt name
    | Array (Ptr typ) ->
        F.fprintf fmt "(*%a)[]" pp typ
    | Array typ ->
        F.fprintf fmt "%a[]" pp typ


  type annotated = {typ: t; attributes: Attr.t list}

  let is_annotated ~f {attributes} = List.exists ~f attributes

  let pp_annotated fmt {typ; attributes} =
    List.iter attributes ~f:(fun attr -> F.fprintf fmt "%a " Attr.pp attr) ;
    pp fmt typ


  let mk_without_attributes typ = {typ; attributes= []}
end

module Ident : sig
  type t [@@deriving equal]

  val to_ssa_var : t -> VarName.t

  val of_int : int -> t

  val to_int : t -> int

  val pp : F.formatter -> t -> unit

  module Map : Caml.Map.S with type key = t

  module Set : Caml.Set.S with type elt = t

  (* We assume idents are totally ordered.
     [next id] returns an ident that is strictly greater than [id] wrt this order. *)
  val next : t -> t

  (* [fresh set] returns an ident that is strictly greater than all idents in [set] *)
  val fresh : Set.t -> t
end = struct
  type t = int [@@deriving equal]

  let to_ssa_var id = Printf.sprintf "__SSA%d" id |> VarName.of_java_name

  let of_int id = id

  let to_int id = id

  let pp fmt id = F.fprintf fmt "n%d" id

  module Map = Caml.Map.Make (Int)
  module Set = Caml.Set.Make (Int)

  let fresh set = 1 + (Set.max_elt_opt set |> Option.value ~default:(-1))

  let next i = i + 1
end

module Const = struct
  type t = Int of Z.t | Null | Str of string | Float of float

  let pp fmt = function
    | Int i ->
        F.pp_print_string fmt (Z.to_string i)
    | Null ->
        F.pp_print_string fmt "null"
    | Str str ->
        F.fprintf fmt "\"%s\"" str
    | Float f ->
        F.pp_print_float fmt f
end

let pp_list_with_comma pp fmt l = Pp.seq ~sep:", " pp fmt l

module ProcSig = struct
  module T = struct
    type t =
      | Hack of {qualified_name: QualifiedProcName.t; arity: int option}
      | Python of {qualified_name: QualifiedProcName.t; arity: int option}
      | Other of {qualified_name: QualifiedProcName.t}
    [@@deriving equal, hash, show {with_path= false}]
  end

  include T

  let to_qualified_procname = function
    | Hack {qualified_name} | Python {qualified_name} | Other {qualified_name} ->
        qualified_name


  let arity = function Hack {arity} | Python {arity} -> arity | Other _ -> None

  let map_arity procsig ~f =
    match procsig with
    | Hack {qualified_name; arity= Some arity} ->
        Hack {qualified_name; arity= Some (f arity)}
    | Python {qualified_name; arity= Some arity} ->
        Python {qualified_name; arity= Some (f arity)}
    | Hack {arity= None} | Python {arity= None} | Other _ ->
        procsig


  let incr_arity procsig = map_arity procsig ~f:(( + ) 1)

  let decr_arity procsig n = map_arity procsig ~f:(fun arity -> arity - n)

  let is_hack_init = function
    | Hack {qualified_name} ->
        QualifiedProcName.is_hack_init qualified_name
    | Python _ | Other _ ->
        false


  module Hashtbl = Hashtbl.Make (T)
end

module ProcDecl = struct
  type t =
    { qualified_name: QualifiedProcName.t
    ; formals_types: Typ.annotated list option
    ; result_type: Typ.annotated
    ; attributes: Attr.t list }

  let formals_or_die ?(context = "<no context>") {qualified_name; formals_types} =
    Option.value_or_thunk formals_types ~default:(fun () ->
        L.die InternalError "List of formals is unknown in %a: %s" QualifiedProcName.pp
          qualified_name context )


  let to_sig {qualified_name; formals_types} = function
    | Some Lang.Hack ->
        ProcSig.Hack {qualified_name; arity= Option.map formals_types ~f:List.length}
    | Some Lang.Python ->
        ProcSig.Python {qualified_name; arity= Option.map formals_types ~f:List.length}
    | Some Lang.Java | None ->
        ProcSig.Other {qualified_name}


  let pp_formals fmt formals =
    match formals with
    | None ->
        F.fprintf fmt "..."
    | Some formals ->
        pp_list_with_comma Typ.pp_annotated fmt formals


  let pp fmt {qualified_name; formals_types; result_type; attributes} =
    List.iter attributes ~f:(fun attr -> F.fprintf fmt "%a " Attr.pp attr) ;
    F.fprintf fmt "%a(%a) : %a" QualifiedProcName.pp qualified_name pp_formals formals_types
      Typ.pp_annotated result_type


  let make_toplevel_name string loc : QualifiedProcName.t =
    let name : ProcName.t = {value= string; loc} in
    {enclosing_class= TopLevel; name}


  let make_builtin_name string loc : QualifiedProcName.t =
    let name : ProcName.t = {value= string; loc} in
    {enclosing_class= Enclosing TypeName.hack_builtin; name}


  let allocate_object_name = make_toplevel_name builtin_allocate Location.Unknown

  let allocate_array_name = make_toplevel_name builtin_allocate_array Location.Unknown

  let lazy_class_initialize_name = make_toplevel_name builtin_lazy_class_initialize Location.Unknown

  let get_lazy_class_name = make_toplevel_name builtin_get_lazy_class Location.Unknown

  let cast_name = make_toplevel_name builtin_cast Location.Unknown

  let generics_constructor = make_builtin_name builtin_generics_constructor Location.Unknown

  let instanceof_name = make_toplevel_name builtin_instanceof Location.Unknown

  let unop_table : (Unop.t * string) list =
    [(Neg, "__sil_neg"); (BNot, "__sil_bnot"); (LNot, "__sil_lnot")]


  let inverse_assoc_list l = List.map l ~f:(fun (a, b) -> (b, a))

  let unop_inverse_table = inverse_assoc_list unop_table

  let of_unop unop =
    let value = List.Assoc.find_exn ~equal:Unop.equal unop_table unop in
    make_toplevel_name value Location.Unknown


  let to_unop ({enclosing_class; name} : QualifiedProcName.t) : Unop.t option =
    match enclosing_class with
    | TopLevel ->
        List.Assoc.find ~equal:String.equal unop_inverse_table name.value
    | _ ->
        None


  let inverse_assoc_list l = List.map l ~f:(fun (a, b) -> (b, a))

  let unop_inverse_table = inverse_assoc_list unop_table

  let binop_table : (Binop.t * string) list =
    [ (PlusA None, "__sil_plusa")
    ; (PlusA (Some IChar), "__sil_plusa_char")
    ; (PlusA (Some ISChar), "__sil_plusschar")
    ; (PlusA (Some IUChar), "__sil_plusa_uchar")
    ; (PlusA (Some IBool), "__sil_plusa_bool")
    ; (PlusA (Some IInt), "__sil_plusa_int")
    ; (PlusA (Some IUInt), "__sil_plusa_uint")
    ; (PlusA (Some IShort), "__sil_plusa_short")
    ; (PlusA (Some IUShort), "__sil_plusa_ushort")
    ; (PlusA (Some ILong), "__sil_plusa_long")
    ; (PlusA (Some IULong), "__sil_plusa_ulong")
    ; (PlusA (Some ILongLong), "__sil_plusa_longlong")
    ; (PlusA (Some IULongLong), "__sil_plusa_ulonglong")
    ; (PlusA (Some I128), "__sil_plusa_128")
    ; (PlusA (Some IU128), "__sil_plusa_u128")
    ; (PlusPI, "__sil_pluspi")
    ; (MinusA None, "__sil_minusa")
    ; (MinusA (Some IChar), "__sil_minusa_char")
    ; (MinusA (Some ISChar), "__sil_minusa_schar")
    ; (MinusA (Some IUChar), "__sil_minusa_uchar")
    ; (MinusA (Some IBool), "__sil_minusa_bool")
    ; (MinusA (Some IInt), "__sil_minusa_int")
    ; (MinusA (Some IUInt), "__sil_minusa_uint")
    ; (MinusA (Some IShort), "__sil_minusa_short")
    ; (MinusA (Some IUShort), "__sil_minusa_ushort")
    ; (MinusA (Some ILong), "__sil_minusa_long")
    ; (MinusA (Some IULong), "__sil_minusa_ulong")
    ; (MinusA (Some ILongLong), "__sil_minusa_longlong")
    ; (MinusA (Some IULongLong), "__sil_minusa_ulonglong")
    ; (MinusA (Some I128), "__sil_minusa_128")
    ; (MinusA (Some IU128), "__sil_minusa_u128")
    ; (MinusPI, "__sil_minuspi")
    ; (MinusPP, "__sil_minuspp")
    ; (Mult None, "__sil_mult")
    ; (Mult (Some IChar), "__sil_mult_char")
    ; (Mult (Some ISChar), "__sil_mult_schar")
    ; (Mult (Some IUChar), "__sil_mult_uchar")
    ; (Mult (Some IBool), "__sil_mult_bool")
    ; (Mult (Some IInt), "__sil_mult_int")
    ; (Mult (Some IUInt), "__sil_mult_uint")
    ; (Mult (Some IShort), "__sil_mult_short")
    ; (Mult (Some IUShort), "__sil_mult_ushort")
    ; (Mult (Some ILong), "__sil_mult_long")
    ; (Mult (Some IULong), "__sil_mult_ulong")
    ; (Mult (Some ILongLong), "__sil_mult_longlong")
    ; (Mult (Some IULongLong), "__sil_mult_ulonglong")
    ; (Mult (Some I128), "__sil_mult_128")
    ; (Mult (Some IU128), "__sil_mult_u128")
    ; (DivI, "__sil_divi")
    ; (DivF, "__sil_divf")
    ; (Mod, "__sil_mod")
    ; (Shiftlt, "__sil_shiftlt")
    ; (Shiftrt, "__sil_shiftrt")
    ; (Lt, "__sil_lt")
    ; (Gt, "__sil_gt")
    ; (Le, "__sil_le")
    ; (Ge, "__sil_ge")
    ; (Eq, "__sil_eq")
    ; (Ne, "__sil_ne")
    ; (BAnd, "__sil_band")
    ; (BXor, "__sil_bxor")
    ; (BOr, "__sil_bor")
    ; (LAnd, "__sil_land")
    ; (LOr, "__sil_lor") ]


  let binop_map = Map.Poly.of_alist_exn binop_table

  let of_binop binop =
    let value = Map.Poly.find_exn binop_map binop in
    make_toplevel_name value Location.Unknown


  let binop_inverse_map = inverse_assoc_list binop_table |> Map.Poly.of_alist_exn

  let is_allocate_object_builtin qualified_name =
    QualifiedProcName.equal allocate_object_name qualified_name


  let is_allocate_array_builtin qualified_name =
    QualifiedProcName.equal allocate_array_name qualified_name


  let is_lazy_class_initialize_builtin qualified_name =
    QualifiedProcName.equal lazy_class_initialize_name qualified_name


  let is_get_lazy_class_builtin qualified_name =
    QualifiedProcName.equal get_lazy_class_name qualified_name


  let is_generics_constructor_builtin = QualifiedProcName.equal generics_constructor

  let is_cast_builtin = QualifiedProcName.equal cast_name

  let is_instanceof_builtin = QualifiedProcName.equal instanceof_name

  let is_type_builtin qualified_name =
    is_allocate_object_builtin qualified_name
    || is_allocate_array_builtin qualified_name
    || is_get_lazy_class_builtin qualified_name
    || is_lazy_class_initialize_builtin qualified_name
    || is_instanceof_builtin qualified_name


  let is_side_effect_free_sil_expr ({enclosing_class; name} as qualified_name : QualifiedProcName.t)
      =
    is_cast_builtin qualified_name
    ||
    match enclosing_class with
    | TopLevel ->
        let name = name.value in
        List.Assoc.mem ~equal:String.equal unop_inverse_table name
        || Map.Poly.mem binop_inverse_map name
    | _ ->
        false


  let is_not_regular_proc proc = is_type_builtin proc || is_side_effect_free_sil_expr proc

  let to_binop ({enclosing_class; name} : QualifiedProcName.t) : Binop.t option =
    match enclosing_class with TopLevel -> Map.Poly.find binop_inverse_map name.value | _ -> None


  let is_curry_invoke {qualified_name= {name}; attributes} =
    String.equal name.value "__invoke" && List.exists attributes ~f:Attr.is_curry


  let is_variadic {formals_types} =
    Option.value_map formals_types ~default:false
      ~f:(List.exists ~f:(Typ.is_annotated ~f:Attr.is_variadic))
end

module Global = struct
  type t = {name: VarName.t; typ: Typ.t; attributes: Attr.t list}

  let pp fmt {name; typ; attributes} =
    let annotated_typ : Typ.annotated = {typ; attributes} in
    F.fprintf fmt "%a: %a" VarName.pp name Typ.pp_annotated annotated_typ
end

module FieldDecl = struct
  type t = {qualified_name: qualified_fieldname; typ: Typ.t; attributes: Attr.t list}

  let pp fmt {qualified_name; typ; attributes} =
    let annotated_typ : Typ.annotated = {typ; attributes} in
    F.fprintf fmt "%a: %a" FieldName.pp qualified_name.name Typ.pp_annotated annotated_typ
end

module Struct = struct
  type t =
    {name: TypeName.t; supers: TypeName.t list; fields: FieldDecl.t list; attributes: Attr.t list}

  let pp fmt {name; supers; fields; attributes} =
    let pp_fields =
      Pp.seq ~print_env:Pp.text_break ~sep:";" (fun fmt -> F.fprintf fmt "%a" FieldDecl.pp)
    in
    let pp_supers =
      Pp.seq ~print_env:Pp.text_break ~sep:"," (fun fmt -> F.fprintf fmt "%a" TypeName.pp)
    in
    List.iter attributes ~f:(fun attr -> F.fprintf fmt "%a " Attr.pp attr) ;
    if List.is_empty supers then
      F.fprintf fmt "%a = {@[<hov>%a@]}" TypeName.pp name pp_fields fields
    else
      F.fprintf fmt "%a extends @[<hov>%a@] = {@[<hov>%a@]}" TypeName.pp name pp_supers supers
        pp_fields fields
end

module Exp = struct
  (* TODO(T133190934) *)
  type call_kind = Virtual | NonVirtual [@@deriving equal]

  type t =
    | Var of Ident.t
    | Load of {exp: t; typ: Typ.t option}
    | Lvar of VarName.t
    | Field of {exp: t; field: qualified_fieldname}
    | Index of t * t
    (*  | Sizeof of sizeof_data *)
    | Const of Const.t
    | Call of {proc: QualifiedProcName.t; args: t list; kind: call_kind}
    | Closure of {proc: QualifiedProcName.t; captured: t list; params: VarName.t list}
    | Apply of {closure: t; args: t list}
    | Typ of Typ.t

  let call_non_virtual proc args = Call {proc; args; kind= NonVirtual}

  let call_virtual proc recv args = Call {proc; args= recv :: args; kind= Virtual}

  let call_sig qualified_name nb_args = function
    | Some Lang.Hack ->
        ProcSig.Hack {qualified_name; arity= Some nb_args}
    | Some Lang.Python ->
        ProcSig.Python {qualified_name; arity= Some nb_args}
    | Some Lang.Java | None ->
        ProcSig.Other {qualified_name}


  let not exp = call_non_virtual (ProcDecl.of_unop Unop.LNot) [exp]

  let cast typ exp = call_non_virtual ProcDecl.cast_name [Typ typ; exp]

  let allocate_object typename =
    Call {proc= ProcDecl.allocate_object_name; args= [Typ (Typ.Struct typename)]; kind= NonVirtual}


  let rec pp fmt = function
    | Apply {closure; args} ->
        F.fprintf fmt "%a%a" pp closure pp_list args
    | Var id ->
        Ident.pp fmt id
    | Load {exp= Lvar x; typ= None} ->
        F.fprintf fmt "%a" VarName.pp x
    | Load {exp; typ= None} ->
        F.fprintf fmt "[%a]" pp exp
    | Load {exp; typ= Some typ} ->
        F.fprintf fmt "[%a:%a]" pp exp Typ.pp typ
    | Lvar x ->
        F.fprintf fmt "&%a" VarName.pp x
    | Field {exp; field} ->
        F.fprintf fmt "%a.%a.%a" pp exp TypeName.pp field.enclosing_class FieldName.pp field.name
    | Index (e1, e2) ->
        F.fprintf fmt "%a[%a]" pp e1 pp e2
    | Const c ->
        Const.pp fmt c
    | Call {proc; args; kind} -> (
      match kind with
      | Virtual -> (
        match args with
        | recv :: other ->
            F.fprintf fmt "%a.%a%a" pp recv QualifiedProcName.pp proc pp_list other
        | _ ->
            L.die InternalError "virtual call with 0 args: %a" QualifiedProcName.pp proc )
      | NonVirtual ->
          F.fprintf fmt "%a%a" QualifiedProcName.pp proc pp_list args )
    | Closure {proc; captured; params} ->
        let captured_and_params =
          captured @ List.map params ~f:(fun varname -> Load {exp= Lvar varname; typ= None})
        in
        F.fprintf fmt "(%a) -> %a(%a)" (pp_list_with_comma VarName.pp) params QualifiedProcName.pp
          proc (pp_list_with_comma pp) captured_and_params
    | Typ typ ->
        F.fprintf fmt "<%a>" Typ.pp typ


  and pp_list fmt l = F.fprintf fmt "(%a)" (pp_list_with_comma pp) l

  let rec do_not_contain_regular_call exp =
    match exp with
    | Var _ | Lvar _ | Const _ | Typ _ ->
        true
    | Load {exp} | Field {exp} ->
        do_not_contain_regular_call exp
    | Index (exp1, exp2) ->
        do_not_contain_regular_call exp1 && do_not_contain_regular_call exp2
    | Call {proc; args} ->
        ProcDecl.is_not_regular_proc proc && List.for_all args ~f:do_not_contain_regular_call
    | Apply {closure; args} ->
        do_not_contain_regular_call closure && List.for_all args ~f:do_not_contain_regular_call
    | Closure {captured} ->
        List.for_all captured ~f:do_not_contain_regular_call


  let vars exp =
    let rec aux acc exp =
      match exp with
      | Var id ->
          Ident.Set.add id acc
      | Lvar _ | Const _ | Typ _ ->
          acc
      | Load {exp} | Field {exp} ->
          aux acc exp
      | Index (exp1, exp2) ->
          aux (aux acc exp1) exp2
      | Apply {closure; args} ->
          List.fold args ~init:(aux acc closure) ~f:aux
      | Call {args} ->
          List.fold args ~init:acc ~f:aux
      | Closure {captured} ->
          List.fold captured ~init:acc ~f:aux
    in
    aux Ident.Set.empty exp
end

module BoolExp = struct
  type t = Exp of Exp.t | Not of t | And of t * t | Or of t * t

  let rec pp fmt bexp =
    match bexp with
    | Exp exp ->
        Exp.pp fmt exp
    | Not bexp ->
        F.fprintf fmt "!(%a)" pp bexp
    | And (bexp1, bexp2) ->
        F.fprintf fmt "(%a) && (%a)" pp bexp1 pp bexp2
    | Or (bexp1, bexp2) ->
        F.fprintf fmt "(%a) || (%a)" pp bexp1 pp bexp2


  let rec do_not_contain_regular_call bexp =
    match bexp with
    | Exp exp ->
        Exp.do_not_contain_regular_call exp
    | Not bexp ->
        do_not_contain_regular_call bexp
    | Or (bexp1, bexp2) | And (bexp1, bexp2) ->
        do_not_contain_regular_call bexp1 && do_not_contain_regular_call bexp2
end

module Instr = struct
  type t =
    | Load of {id: Ident.t; exp: Exp.t; typ: Typ.t option; loc: Location.t}
    | Store of {exp1: Exp.t; typ: Typ.t option; exp2: Exp.t; loc: Location.t}
    | Prune of {exp: Exp.t; loc: Location.t}
    | Let of {id: Ident.t; exp: Exp.t; loc: Location.t}

  let loc = function Load {loc} | Store {loc} | Prune {loc} | Let {loc} -> loc

  let pp fmt = function
    | Load {id; exp; typ= None} ->
        F.fprintf fmt "%a = load %a" Ident.pp id Exp.pp exp
    | Load {id; exp; typ= Some typ} ->
        F.fprintf fmt "%a:%a = load %a" Ident.pp id Typ.pp typ Exp.pp exp
    | Store {exp1; typ= None; exp2} ->
        F.fprintf fmt "store %a <- %a" Exp.pp exp1 Exp.pp exp2
    | Store {exp1; typ= Some typ; exp2} ->
        F.fprintf fmt "store %a <- %a:%a" Exp.pp exp1 Exp.pp exp2 Typ.pp typ
    | Prune {exp} ->
        F.fprintf fmt "prune %a" Exp.pp exp
    | Let {id; exp} ->
        F.fprintf fmt "%a = %a" Ident.pp id Exp.pp exp


  (* to be ready, an instruction should satisfy 2 properties:
      1) regular calls should only appear as top level expr of Let instruction
      2) Let instruction should only have this kind of expression as argument *)
  let is_ready_for_to_sil_conversion i =
    match i with
    | Load {exp} ->
        Exp.do_not_contain_regular_call exp
    | Store {exp1; exp2} ->
        Exp.do_not_contain_regular_call exp1 && Exp.do_not_contain_regular_call exp2
    | Prune {exp} ->
        Exp.do_not_contain_regular_call exp
    | Let {exp= Call {proc; args= []}} when ProcDecl.is_type_builtin proc ->
        true
    | Let {exp= Call {proc; args}} ->
        (not (ProcDecl.is_not_regular_proc proc))
        && List.for_all args ~f:Exp.do_not_contain_regular_call
    | Let {exp= _} ->
        false
end

module Terminator = struct
  type node_call = {label: NodeName.t; ssa_args: Exp.t list}

  type t =
    | If of {bexp: BoolExp.t; then_: t; else_: t}
    | Ret of Exp.t
    | Jump of node_call list
    | Throw of Exp.t
    | Unreachable

  let pp_block_call fmt {label; ssa_args} =
    match ssa_args with
    | [] ->
        NodeName.pp fmt label
    | _ ->
        F.fprintf fmt "%a(%a)" NodeName.pp label (pp_list_with_comma Exp.pp) ssa_args


  let rec pp fmt = function
    | If {bexp; then_; else_} ->
        F.fprintf fmt "if %a then %a else %a" BoolExp.pp bexp pp then_ pp else_
    | Ret e ->
        F.fprintf fmt "ret %a" Exp.pp e
    | Jump l ->
        F.fprintf fmt "jmp %a" (pp_list_with_comma pp_block_call) l
    | Throw e ->
        F.fprintf fmt "throw %a" Exp.pp e
    | Unreachable ->
        F.pp_print_string fmt "unreachable"


  let rec do_not_contain_regular_call t =
    match t with
    | If {bexp; then_; else_} ->
        BoolExp.do_not_contain_regular_call bexp
        && do_not_contain_regular_call then_ && do_not_contain_regular_call else_
    | Ret exp | Throw exp ->
        Exp.do_not_contain_regular_call exp
    | Jump _ | Unreachable ->
        true
end

module Node = struct
  type t =
    { label: NodeName.t
    ; ssa_parameters: (Ident.t * Typ.t) list
    ; exn_succs: NodeName.t list
    ; last: Terminator.t
    ; instrs: Instr.t list
    ; last_loc: Location.t
    ; label_loc: Location.t }

  (* see the specification of Instr.is_ready_for_to_sil_conversion above *)
  let is_ready_for_to_sil_conversion node =
    Terminator.do_not_contain_regular_call node.last
    && List.for_all node.instrs ~f:Instr.is_ready_for_to_sil_conversion


  let pp fmt node =
    let pp_label_with_ssa_params fmt =
      if List.is_empty node.ssa_parameters then F.fprintf fmt "#%a:" NodeName.pp node.label
      else
        let pp_param fmt (id, typ) = F.fprintf fmt "%a: %a" Ident.pp id Typ.pp typ in
        F.fprintf fmt "#%a(%a):" NodeName.pp node.label (pp_list_with_comma pp_param)
          node.ssa_parameters
    in
    F.fprintf fmt "@\n@[<v 4>%t" pp_label_with_ssa_params ;
    List.iter ~f:(F.fprintf fmt "@\n%a" Instr.pp) node.instrs ;
    F.fprintf fmt "@\n%a" Terminator.pp node.last ;
    if not (List.is_empty node.exn_succs) then
      F.fprintf fmt "@\n.handlers %a" (pp_list_with_comma NodeName.pp) node.exn_succs ;
    F.fprintf fmt "@\n@]"


  let equal node1 node2 = NodeName.equal node1.label node2.label
end

module ProcDesc = struct
  type t =
    { procdecl: ProcDecl.t
    ; nodes: Node.t list
    ; start: NodeName.t
    ; params: VarName.t list
    ; locals: (VarName.t * Typ.annotated) list
    ; exit_loc: Location.t }

  let is_ready_for_to_sil_conversion {nodes} =
    List.for_all nodes ~f:Node.is_ready_for_to_sil_conversion


  let formals {procdecl; _} = ProcDecl.formals_or_die procdecl ~context:"ProcDesc must have formals"

  let pp_signature fmt ({procdecl; params; _} as t) =
    let pp fmt (typ, id) = F.fprintf fmt "%a: %a" VarName.pp id Typ.pp_annotated typ in
    List.iter procdecl.attributes ~f:(fun attr -> F.fprintf fmt "%a " Attr.pp attr) ;
    let formals_types = formals t in
    match List.zip formals_types params with
    | Ok args ->
        F.fprintf fmt "%a(%a) : %a" QualifiedProcName.pp procdecl.qualified_name
          (pp_list_with_comma pp) args Typ.pp_annotated procdecl.result_type
    | _ ->
        L.die InternalError
          "Textual printing error: params has length %d and formals_types has length %d"
          (List.length params) (List.length formals_types)


  let pp fmt t =
    F.fprintf fmt "@[<v 2>define %a {" pp_signature t ;
    let pp_local fmt (var, annotated_typ) =
      F.fprintf fmt "%a: %a" VarName.pp var Typ.pp_annotated annotated_typ
    in
    if not (List.is_empty t.locals) then
      F.fprintf fmt "@\n@[<v 4>local %a@]" (pp_list_with_comma pp_local) t.locals ;
    List.iter ~f:(F.fprintf fmt "%a" Node.pp) t.nodes ;
    F.fprintf fmt "@]\n}@\n@\n"
end

module Body = struct
  type t = {nodes: Node.t list; locals: (VarName.t * Typ.annotated) list}

  let dummy loc =
    let node : Node.t =
      { label= {value= "entry"; loc}
      ; ssa_parameters= []
      ; exn_succs= []
      ; last= Terminator.Ret (Exp.Const Const.Null)
      ; instrs= []
      ; last_loc= loc
      ; label_loc= loc }
    in
    {nodes= [node]; locals= []}
end

module SsaVerification = struct
  type error = {id: Ident.t; locations: Location.Set.t}

  let pp_error fmt {id; locations} =
    let pp_location fmt loc = F.fprintf fmt "[%a]" Location.pp loc in
    F.fprintf fmt "ident %a is defined more than once at locations %a" Ident.pp id
      (F.pp_print_list ~pp_sep:(fun fmt () -> F.pp_print_string fmt ", ") pp_location)
      (Location.Set.elements locations)


  let error_to_transform_error error =
    let primary_loc =
      Location.Set.choose_opt error.locations |> Option.value ~default:Location.Unknown
    in
    {loc= primary_loc; msg= lazy (F.asprintf "%a" pp_error error)}


  let run (pdesc : ProcDesc.t) =
    let collect seen id loc =
      match Ident.Map.find_opt id seen with
      | None ->
          Ident.Map.add id (Location.Set.singleton loc) seen
      | Some locations ->
          Ident.Map.add id (Location.Set.add loc locations) seen
    in
    let collect_defs_in_instr seen (instr : Instr.t) =
      match instr with
      | Load {id; loc} | Let {id; loc} ->
          collect seen id loc
      | Store _ | Prune _ ->
          seen
    in
    let collect_defs_in_node seen (node : Node.t) =
      List.fold node.instrs ~init:seen ~f:collect_defs_in_instr
    in
    let seen = List.fold pdesc.nodes ~f:collect_defs_in_node ~init:Ident.Map.empty in
    let errors =
      Ident.Map.fold
        (fun id locations errors ->
          if Location.Set.cardinal locations > 1 then {id; locations} :: errors else errors )
        seen []
    in
    if not (List.is_empty errors) then
      let transform_errors = List.map errors ~f:error_to_transform_error in
      raise (TextualTransformError transform_errors)
end

module Module = struct
  type decl =
    | Global of Global.t
    | Struct of Struct.t
    | Procdecl of ProcDecl.t
    | Proc of ProcDesc.t

  type t = {attrs: Attr.t list; decls: decl list; sourcefile: SourceFile.t}

  let lang {attrs} =
    let lang_attr =
      List.find attrs ~f:(fun (attr : Attr.t) -> String.equal attr.name Attr.source_language)
    in
    lang_attr |> Option.bind ~f:(fun x -> Attr.values x |> List.hd |> Option.bind ~f:Lang.of_string)


  let pp_attr fmt attr = F.fprintf fmt "%a@\n@\n" Attr.pp attr

  let pp_decl fmt = function
    | Global global ->
        F.fprintf fmt "global %a@\n@\n" Global.pp global
    | Proc pdesc ->
        ProcDesc.pp fmt pdesc
    | Procdecl procdecl ->
        F.fprintf fmt "declare %a@\n@\n" ProcDecl.pp procdecl
    | Struct struct_ ->
        F.fprintf fmt "type %a@\n@\n" Struct.pp struct_


  let pp fmt module_ =
    List.iter ~f:(pp_attr fmt) module_.attrs ;
    List.iter ~f:(pp_decl fmt) module_.decls
end

exception SpecialSyntaxError of Location.t * string
