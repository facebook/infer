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

exception ToSilTransformationError of (F.formatter -> unit -> unit)

module Location = struct
  type t = Known of {line: int; col: int} | Unknown [@@deriving compare]

  let known ~line ~col = Known {line; col}

  let pp fmt = function
    | Known {line; col} ->
        F.fprintf fmt "line %d, column %d" line col
    | Unknown ->
        F.fprintf fmt "<unknown location>"


  let to_sil file = function
    | Known {line; col} ->
        Location.{line; col; file}
    | Unknown ->
        Location.none file


  let of_sil ({line; col} : Location.t) =
    if Int.(line = -1 && col = -1) then Known {line; col} else Unknown


  module Set = Caml.Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end

module type NAME = sig
  type t = {value: string; loc: Location.t}
end

(* this signature will not be exported in .mli *)
module type COMMON_NAME = sig
  type t = {value: string; loc: Location.t} [@@deriving equal, hash]

  val of_java_name : string -> t

  val pp : F.formatter -> t -> unit

  module Hashtbl : Hashtbl.S with type key = t

  module Map : Caml.Map.S with type key = t

  module Set : Caml.Set.S with type elt = t
end

module Name : COMMON_NAME = struct
  type t = {value: string; loc: Location.t [@compare.ignore] [@equal.ignore] [@hash.ignore]}
  [@@deriving compare, equal, hash]

  let replace_dot_with_2colons str = String.substr_replace_all str ~pattern:"." ~with_:"::"

  let of_java_name str = {value= replace_dot_with_2colons str; loc= Location.Unknown}

  let pp fmt name = F.pp_print_string fmt name.value

  module Hashtbl = Hashtbl.Make (struct
    type nonrec t = t

    let equal = equal

    let hash = hash
  end)

  module Map = Caml.Map.Make (struct
    type nonrec t = t

    let compare = compare
  end)

  module Set = Caml.Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end

module ProcBaseName : COMMON_NAME = Name

module FieldBaseName : COMMON_NAME = Name

let builtin_allocate_prefix = "__sil_allocate_"

let builtin_allocate_array = "__sil_allocate_array"

module TypeName : sig
  include COMMON_NAME

  val of_sil_typ_name : Typ.Name.t -> t

  val to_java_sil : t -> Typ.Name.t

  val java_lang_object : Typ.Name.t

  val allocate_buitin_to_java_sil : ProcBaseName.t -> Typ.Name.t
end = struct
  include Name

  let of_sil_typ_name (tname : Typ.Name.t) =
    match tname with
    | JavaClass name ->
        of_java_name (JavaClassName.to_string name)
    | _ ->
        L.die InternalError "Textual conversion: only Java expected here"


  let replace_2colons_with_dot str = String.substr_replace_all str ~pattern:"::" ~with_:"."

  let string_to_java_sil string : Typ.Name.t =
    JavaClass (replace_2colons_with_dot string |> JavaClassName.from_string)


  let java_lang_object : Typ.Name.t = JavaClass (JavaClassName.from_string "java.lang.Object")

  let to_java_sil {value} : Typ.Name.t = string_to_java_sil value

  let allocate_buitin_to_java_sil (proc : ProcBaseName.t) : Typ.Name.t =
    let prefix_length = String.length builtin_allocate_prefix in
    let length = String.length proc.value in
    let classname =
      String.sub proc.value ~pos:prefix_length ~len:(length - prefix_length)
      |> replace_2colons_with_dot
    in
    string_to_java_sil classname
end

module VarName : COMMON_NAME = Name

module NodeName : COMMON_NAME = Name

module SilTyp = Typ

module Typ = struct
  type t = Int | Float | Null | Void | Ptr of t | Struct of TypeName.t | Array of t

  let rec to_sil typ : Typ.t =
    let quals = Typ.mk_type_quals () in
    let desc : Typ.desc =
      match typ with
      | Int ->
          Tint IInt (* FIXME: add size *)
      | Null ->
          Tint IInt
      | Float ->
          Tfloat FFloat (* FIXME: add size *)
      | Void ->
          Tvoid
      | Ptr t ->
          Tptr (to_sil t, Pk_pointer)
      | Struct name ->
          Typ.Tstruct (TypeName.to_java_sil name)
      | Array t ->
          Tarray {elt= to_sil t; length= None; stride= None}
    in
    {desc; quals}


  let rec of_sil ({desc} : Typ.t) =
    match desc with
    | Tint _ ->
        Int (* TODO: check size and make Textual.Tint size aware *)
    | Tfloat _ ->
        Float
    | Tvoid ->
        Void
    | Tfun ->
        L.die InternalError "Textual conversion: Tfun type does not appear in Java"
    | Tptr (t, Pk_pointer) ->
        Ptr (of_sil t)
    | Tptr (_, _) ->
        L.die InternalError "Textual conversion: this ptr type should not appear in Java"
    | Tstruct name ->
        Struct (TypeName.of_sil_typ_name name)
    | TVar _ ->
        L.die InternalError "Textual conversion: TVar type should not appear in Java"
    | Tarray {elt} ->
        Array (of_sil elt)


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
end

module Ident : sig
  type t [@@deriving equal]

  val to_sil : t -> Ident.t

  val to_ssa_var : t -> VarName.t

  val of_int : int -> t

  val of_sil : Ident.t -> t

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

  let to_sil id = Ident.create Ident.knormal id
  (* TODO: check the Ident generator is ready *)

  let to_ssa_var id = Printf.sprintf "__SSA%d" id |> VarName.of_java_name

  let of_int id = id

  let of_sil (id : Ident.t) =
    if not (Ident.is_normal id) then
      L.die InternalError "Textual conversion: onlyt normal ident should appear in Java"
    else Ident.get_stamp id


  let pp fmt id = F.fprintf fmt "n%d" id

  module Map = Caml.Map.Make (Int)
  module Set = Caml.Set.Make (Int)

  let fresh set = 1 + (Set.max_elt_opt set |> Option.value ~default:(-1))

  let next i = i + 1
end

module SilConst = Const

module Const = struct
  type t = Int of Z.t | Null | Str of string | Float of float

  let to_sil const : Const.t =
    match const with
    | Int z ->
        Cint (IntLit.of_big_int z)
    | Null ->
        Cint IntLit.zero
    | Str s ->
        Cstr s
    | Float f ->
        Cfloat f


  let of_sil (const : Const.t) =
    match const with
    | Cint i ->
        Int (IntLit.to_big_int i)
    | Cstr str ->
        Str str
    | Cfloat f ->
        Float f
    | Cfun _ ->
        L.die InternalError
          "Textual conversion: Cfun constant should not appear at this position in Java"
    | Cclass _ ->
        L.die InternalError
          "Textual conversion: class constant is not supported yet (see T127289235)"


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

module Lang = struct
  type t = Java | Hack [@@deriving equal]

  let of_string s =
    match String.lowercase s with "java" -> Some Java | "hack" -> Some Hack | _ -> None


  let to_string = function Java -> "java" | Hack -> "hack"
end

let pp_list_with_comma pp fmt l = Pp.seq ~sep:", " pp fmt l

module SilProcname = Procname

module Procname = struct
  type kind = Virtual | NonVirtual [@@deriving equal]

  type enclosing_class = TopLevel | Enclosing of TypeName.t [@@deriving equal, hash]

  type qualified_name = {enclosing_class: enclosing_class; name: ProcBaseName.t}
  [@@deriving equal, hash]

  type t =
    {qualified_name: qualified_name; formals_types: Typ.t list; result_type: Typ.t; kind: kind}

  let toplevel_classname = "$TOPLEVEL$CLASS$"

  let pp_enclosing_class fmt = function
    | TopLevel ->
        ()
    | Enclosing tname ->
        F.fprintf fmt "%a." TypeName.pp tname


  let pp_qualified_name fmt {enclosing_class; name} =
    F.fprintf fmt "%a%a" pp_enclosing_class enclosing_class ProcBaseName.pp name


  let pp fmt {qualified_name; formals_types; result_type} =
    F.fprintf fmt "%a(%a) : %a" pp_qualified_name qualified_name (pp_list_with_comma Typ.pp)
      formals_types Typ.pp result_type


  let pp_with_params params fmt {qualified_name; formals_types; result_type} =
    let pp fmt (typ, id) = F.fprintf fmt "%a: %a" VarName.pp id Typ.pp typ in
    match List.zip formals_types params with
    | Ok args ->
        F.fprintf fmt "%a(%a) : %a" pp_qualified_name qualified_name (pp_list_with_comma pp) args
          Typ.pp result_type
    | _ ->
        L.die InternalError
          "Textual printing error: params has size %d and formals_types has size %d"
          (List.length params) (List.length formals_types)


  let of_sil (pname : Procname.t) =
    match pname with
    | Java jpname ->
        let kind =
          if Procname.Java.is_static jpname then NonVirtual
          else Virtual (* FIXME: we do not handle virtuall call yet *)
        in
        let enclosing_class =
          Enclosing (TypeName.of_java_name (Procname.Java.get_class_name jpname))
        in
        let name = Procname.Java.get_method jpname |> ProcBaseName.of_java_name in
        let qualified_name : qualified_name = {enclosing_class; name} in
        let formals_types = Procname.Java.get_parameters jpname |> List.map ~f:Typ.of_sil in
        let formals_types =
          if Procname.Java.is_static jpname then formals_types
          else
            let typ = Procname.Java.get_class_type_name jpname in
            let this_type = Typ.(Ptr (Struct (TypeName.of_sil_typ_name typ))) in
            this_type :: formals_types
        in
        let result_type = Procname.Java.get_return_typ jpname |> Typ.of_sil in
        (* FIXME when adding inheritance *)
        {qualified_name; formals_types; result_type; kind}
    | _ ->
        L.die InternalError "Non-Java procname %a should not appear in Java mode" Procname.describe
          pname


  let make_toplevel_name name : qualified_name = {enclosing_class= TopLevel; name}

  let make_builtin ~name ~formals_types ~result_type =
    let qualified_name : qualified_name = make_toplevel_name name in
    {qualified_name; formals_types; result_type; kind= NonVirtual}


  let make_allocate (tname : TypeName.t) =
    let name : ProcBaseName.t =
      {value= builtin_allocate_prefix ^ tname.value; loc= Location.Unknown}
    in
    make_builtin ~name ~formals_types:[] ~result_type:(Ptr (Struct tname))


  let make_allocate_array (typ : Typ.t) =
    (* TODO: make usage of the content type in the procedure name *)
    let name : ProcBaseName.t = {value= builtin_allocate_array; loc= Location.Unknown} in
    make_builtin ~name ~formals_types:[] ~result_type:(Ptr (Array typ))


  let unop_table : (Unop.t * string) list =
    [(Neg, "__sil_neg"); (BNot, "__sil_bnot"); (LNot, "__sil_lnot")]


  let inverse_assoc_list l = List.map l ~f:(fun (a, b) -> (b, a))

  let unop_inverse_table = inverse_assoc_list unop_table

  let of_unop unop =
    let value = List.Assoc.find_exn ~equal:Unop.equal unop_table unop in
    let name : ProcBaseName.t = {value; loc= Location.Unknown} in
    make_toplevel_name name


  let to_unop ({enclosing_class; name} : qualified_name) : Unop.t option =
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
    let name : ProcBaseName.t = {value; loc= Location.Unknown} in
    make_toplevel_name name


  let binop_inverse_map = inverse_assoc_list binop_table |> Map.Poly.of_alist_exn

  let is_allocate_object_builtin ({enclosing_class; name} : qualified_name) =
    match enclosing_class with
    | TopLevel ->
        String.is_prefix ~prefix:builtin_allocate_prefix name.value
    | _ ->
        false


  let is_allocate_array_builtin ({enclosing_class; name} : qualified_name) =
    match enclosing_class with
    | TopLevel ->
        String.equal builtin_allocate_array name.value
    | _ ->
        false


  let is_allocate_builtin qualified_name =
    is_allocate_object_builtin qualified_name || is_allocate_array_builtin qualified_name


  let is_sil_instr ({enclosing_class; name} : qualified_name) =
    match enclosing_class with
    | TopLevel ->
        let name = name.value in
        List.Assoc.mem ~equal:String.equal unop_inverse_table name
        || Map.Poly.mem binop_inverse_map name
    | _ ->
        false


  let is_not_regular_proc proc = is_allocate_builtin proc || is_sil_instr proc

  let to_binop ({enclosing_class; name} : qualified_name) : Binop.t option =
    match enclosing_class with TopLevel -> Map.Poly.find binop_inverse_map name.value | _ -> None


  let to_sil lang {qualified_name; formals_types; result_type} : Procname.t =
    let method_name = qualified_name.name.ProcBaseName.value in
    match (lang : Lang.t) with
    | Java ->
        let class_name =
          TypeName.to_java_sil
            ( match qualified_name.enclosing_class with
            | TopLevel ->
                TypeName.of_java_name toplevel_classname
            | Enclosing tname ->
                tname )
        in
        let result_type = Typ.to_sil result_type in
        let return_type = Some result_type in
        let formals_types = List.map ~f:Typ.to_sil formals_types in
        let kind = Procname.Java.Non_Static (* FIXME when handling inheritance *) in
        Procname.make_java ~class_name ~return_type ~method_name ~parameters:formals_types ~kind
    | Hack ->
        let class_name =
          match qualified_name.enclosing_class with
          | TopLevel ->
              None
          | Enclosing name ->
              Some name.value
        in
        Procname.make_hack ~class_name ~function_name:method_name


  let kind_to_sil_callflag = function
    | Virtual ->
        {CallFlags.default with cf_virtual= true}
    | _ ->
        CallFlags.default
end

let instr_is_return = function Sil.Store {e1= Lvar v} -> Pvar.is_return v | _ -> false

module SilPvar = Pvar

module Pvar = struct
  type kind = Global | Local of Procname.t

  type t = {name: VarName.t; kind: kind}

  let is_global pvar = match pvar.kind with Global -> true | _ -> false

  let to_sil lang {name; kind} =
    let mangled = Mangled.from_string name.value in
    match kind with
    | Global ->
        Pvar.mk_global mangled
    | Local procname ->
        let pname = Procname.to_sil lang procname in
        Pvar.mk mangled pname


  let of_sil (pvar : Pvar.t) =
    let name = Pvar.get_name pvar |> Mangled.to_string |> VarName.of_java_name in
    let kind =
      if Pvar.is_global pvar then Global
      else if Pvar.is_local pvar then
        match Pvar.get_declaring_function pvar with
        | Some pname ->
            Local (Procname.of_sil pname)
        | _ ->
            L.die InternalError
              "Textual conversion of pvar: infeasible case because the var is local"
      else
        L.die InternalError
          "Textual conversion of pvar: in Java frontend, only local and global are generated"
    in
    {name; kind}
end

module Fieldname = struct
  type t = {name: FieldBaseName.t; typ: Typ.t; enclosing_type: TypeName.t}

  let to_sil {name; enclosing_type} =
    Fieldname.make (TypeName.to_java_sil enclosing_type) name.value


  let of_sil f typ =
    let name = Fieldname.get_field_name f |> FieldBaseName.of_java_name in
    let enclosing_type = Fieldname.get_class_name f |> TypeName.of_sil_typ_name in
    {name; typ; enclosing_type}


  let pp fmt {name; typ} = F.fprintf fmt "%a: %a" FieldBaseName.pp name Typ.pp typ
end

module Struct = struct
  type t =
    { name: TypeName.t
    ; fields: Fieldname.t list
    ; methods: Procname.t list (* currently only the toplevel class will contain methods *) }

  let to_sil lang tenv {name; fields; methods} =
    let name = TypeName.to_java_sil name in
    let fields =
      List.map fields ~f:(fun fname ->
          (Fieldname.to_sil fname, Typ.to_sil fname.Fieldname.typ, Annot.Item.empty) )
    in
    (* FIXME: generate static fields *)
    let methods = List.map methods ~f:(Procname.to_sil lang) in
    Tenv.mk_struct tenv ~fields ~methods name |> ignore


  let of_sil name sil_struct =
    let of_sil_field (fieldname, typ, _) =
      let typ = Typ.of_sil typ in
      Fieldname.of_sil fieldname typ
    in
    let fields = sil_struct.Struct.fields @ sil_struct.Struct.statics in
    let fields = List.map ~f:of_sil_field fields in
    {name; fields; methods= []}


  let pp fmt {name; fields} =
    let pp_fields =
      Pp.seq ~print_env:Pp.text_break ~sep:";" (fun fmt -> F.fprintf fmt " %a" Fieldname.pp)
    in
    F.fprintf fmt "%a = {@[<hov>%a@] }" TypeName.pp name pp_fields fields
end

module Decls = struct
  (* We do not export this module. We record here each name to a more elaborate object *)

  module QualifiedNameHashtbl = Hashtbl.Make (struct
    type t = Procname.qualified_name

    let equal = Procname.equal_qualified_name

    let hash = Procname.hash_qualified_name
  end)

  type t =
    { globals: Pvar.t VarName.Hashtbl.t
    ; procnames: Procname.t QualifiedNameHashtbl.t
    ; structs: Struct.t TypeName.Hashtbl.t
    ; sourcefile: SourceFile.t }

  let init sourcefile =
    { globals= VarName.Hashtbl.create 17
    ; procnames= QualifiedNameHashtbl.create 17
    ; structs= TypeName.Hashtbl.create 17
    ; sourcefile }


  let declare_global decls (pvar : Pvar.t) =
    VarName.Hashtbl.replace decls.globals pvar.name pvar |> ignore


  let declare_procname decls (pname : Procname.t) =
    QualifiedNameHashtbl.replace decls.procnames pname.qualified_name pname |> ignore


  let declare_struct decls (s : Struct.t) =
    TypeName.Hashtbl.replace decls.structs s.name s |> ignore


  let declare_struct_from_tenv decls tenv tname =
    match TypeName.Hashtbl.find_opt decls.structs tname with
    | Some _ ->
        ()
    | None -> (
        let sil_tname = TypeName.to_java_sil tname in
        match Tenv.lookup tenv sil_tname with
        | None ->
            L.die InternalError "Java type %a not found in type environment" SilTyp.Name.pp
              sil_tname
        | Some struct_ ->
            Struct.of_sil tname struct_ |> declare_struct decls )


  let is_fieldname_declared decls (tname : TypeName.t) (fname : FieldBaseName.t) =
    match TypeName.Hashtbl.find_opt decls.structs tname with
    | None ->
        false
    | Some struct_ ->
        List.exists struct_.fields ~f:(fun {Fieldname.name} -> FieldBaseName.equal name fname)


  let get_global decls vname = VarName.Hashtbl.find_opt decls.globals vname

  let get_fieldname decls (tname : TypeName.t) (fname : FieldBaseName.t) =
    let open IOption.Let_syntax in
    let* strct = TypeName.Hashtbl.find_opt decls.structs tname in
    List.find strct.Struct.fields ~f:(fun {Fieldname.name} -> FieldBaseName.equal name fname)


  let get_procname decls qualified_name =
    QualifiedNameHashtbl.find_opt decls.procnames qualified_name


  let is_procname_declared decls proc = get_procname decls proc |> Option.is_some

  let fold_globals decls ~init ~f =
    VarName.Hashtbl.fold (fun key data x -> f x key data) decls.globals init


  let fold_procnames decls ~init ~f =
    QualifiedNameHashtbl.fold (fun _ procname x -> f x procname) decls.procnames init


  let fold_structs decls ~init ~f =
    TypeName.Hashtbl.fold (fun key data x -> f x key data) decls.structs init
end

module SilExp = Exp

module Exp = struct
  type t =
    | Var of Ident.t
    | Lvar of VarName.t
    | Field of {exp: t; tname: TypeName.t; fname: FieldBaseName.t}
    | Index of t * t
    (*  | Sizeof of sizeof_data *)
    | Const of Const.t
    | Call of {proc: Procname.qualified_name; args: t list}
    | Cast of Typ.t * t

  let not exp = Call {proc= Procname.of_unop Unop.LNot; args= [exp]}

  let rec of_sil decls tenv (e : Exp.t) =
    match e with
    | Var id ->
        Var (Ident.of_sil id)
    | UnOp (o, e, _) ->
        let pname = Procname.of_unop o in
        Call {proc= pname; args= [of_sil decls tenv e]}
    | BinOp (o, e1, e2) ->
        let pname = Procname.of_binop o in
        Call {proc= pname; args= [of_sil decls tenv e1; of_sil decls tenv e2]}
    | Exn _ ->
        L.die InternalError "Exp Exn translation not supported"
    | Closure _ ->
        L.die InternalError "Exp Closure translation not supported"
    | Const c ->
        Const (Const.of_sil c)
    | Cast (typ, e) ->
        Cast (Typ.of_sil typ, of_sil decls tenv e)
    | Lvar pvar ->
        let pvar = Pvar.of_sil pvar in
        if Pvar.is_global pvar then Decls.declare_global decls pvar ;
        Lvar pvar.name
    | Lfield (e, f, typ) ->
        let typ = Typ.of_sil typ in
        let fieldname = Fieldname.of_sil f typ in
        let () = Decls.declare_struct_from_tenv decls tenv fieldname.enclosing_type in
        Field {exp= of_sil decls tenv e; fname= fieldname.name; tname= fieldname.enclosing_type}
    | Lindex (e1, e2) ->
        Index (of_sil decls tenv e1, of_sil decls tenv e2)
    | Sizeof _ ->
        L.die InternalError "Sizeof expression should note appear here, please report"


  let rec pp fmt = function
    | Var id ->
        Ident.pp fmt id
    | Lvar x ->
        F.fprintf fmt "&%a" VarName.pp x
    | Field {exp; tname; fname} ->
        F.fprintf fmt "%a.%a.%a" pp exp TypeName.pp tname FieldBaseName.pp fname
    | Index (e1, e2) ->
        F.fprintf fmt "%a[%a]" pp e1 pp e2
    | Const c ->
        Const.pp fmt c
    | Call {proc; args} ->
        Procname.pp_qualified_name fmt proc ;
        pp_list fmt args
    | Cast (typ, e) ->
        F.fprintf fmt "(%a: %a)" pp e Typ.pp typ


  and pp_list fmt l = F.fprintf fmt "(%a)" (pp_list_with_comma pp) l

  let rec do_not_contain_regular_call exp =
    match exp with
    | Var _ | Lvar _ | Const _ ->
        true
    | Field {exp} | Cast (_, exp) ->
        do_not_contain_regular_call exp
    | Index (exp1, exp2) ->
        do_not_contain_regular_call exp1 && do_not_contain_regular_call exp2
    | Call {proc; args} ->
        Procname.is_not_regular_proc proc && List.for_all args ~f:do_not_contain_regular_call


  let to_sil lang decls_env procname exp =
    let rec aux e : Exp.t =
      match e with
      | Var id ->
          Var (Ident.to_sil id)
      | Lvar name ->
          let pvar : Pvar.t =
            match Decls.get_global decls_env name with
            | Some pvar ->
                pvar
            | None ->
                {name; kind= Local procname}
          in
          Lvar (Pvar.to_sil lang pvar)
      | Field {exp; tname; fname} -> (
        match Decls.get_fieldname decls_env tname fname with
        | None ->
            L.die InternalError "field %a has not been declared" FieldBaseName.pp fname
        | Some field ->
            Lfield (aux exp, Fieldname.to_sil field, Typ.to_sil field.typ) )
      | Index (exp1, exp2) ->
          Lindex (aux exp1, aux exp2)
      | Const const ->
          Const (Const.to_sil const)
      | Call {proc; args} -> (
        match
          (Decls.get_procname decls_env proc, Procname.to_unop proc, Procname.to_binop proc, args)
        with
        | Some _, None, None, _ ->
            raise
              (ToSilTransformationError
                 (fun fmt () -> F.fprintf fmt "%a contains a call inside a sub-expression" pp exp)
              )
        | None, Some unop, None, [exp] ->
            UnOp (unop, aux exp, Some (Typ.to_sil Int)) (* FIXME: fix the typ *)
        | None, None, Some binop, [exp1; exp2] ->
            BinOp (binop, aux exp1, aux exp2)
        | _, _, _, _ ->
            L.die InternalError "Internal error: procname %a has an unexpected property"
              Procname.pp_qualified_name proc
            (* FIXME: transform instruction to put call at head of expressions *) )
      | Cast (typ, exp) ->
          Cast (Typ.to_sil typ, aux exp)
    in
    aux exp


  let vars exp =
    let rec aux acc exp =
      match exp with
      | Var id ->
          Ident.Set.add id acc
      | Lvar _ | Const _ ->
          acc
      | Field {exp} ->
          aux acc exp
      | Index (exp1, exp2) ->
          aux (aux acc exp1) exp2
      | Call {args} ->
          List.fold args ~init:acc ~f:aux
      | Cast (_, exp) ->
          aux acc exp
    in
    aux Ident.Set.empty exp


  let rec subst_one exp ~id ~by =
    match exp with
    | Var id' when Ident.equal id id' ->
        by
    | Var _ | Lvar _ | Const _ ->
        exp
    | Field f ->
        Field {f with exp= subst_one f.exp ~id ~by}
    | Index (exp1, exp2) ->
        Index (subst_one exp1 ~id ~by, subst_one exp2 ~id ~by)
    | Call f ->
        Call {f with args= List.map f.args ~f:(fun exp -> subst_one exp ~id ~by)}
    | Cast (typ, exp) ->
        Cast (typ, subst_one exp ~id ~by)


  let rec subst exp eqs =
    match exp with
    | Var id ->
        Ident.Map.find_opt id eqs |> Option.value ~default:exp
    | Lvar _ | Const _ ->
        exp
    | Field f ->
        Field {f with exp= subst f.exp eqs}
    | Index (exp1, exp2) ->
        Index (subst exp1 eqs, subst exp2 eqs)
    | Call f ->
        Call {f with args= List.map f.args ~f:(fun exp -> subst exp eqs)}
    | Cast (typ, exp) ->
        Cast (typ, subst exp eqs)
end

module Instr = struct
  type t =
    | Load of {id: Ident.t; exp: Exp.t; typ: Typ.t; loc: Location.t}
    | Store of {exp1: Exp.t; typ: Typ.t; exp2: Exp.t; loc: Location.t}
    | Prune of {exp: Exp.t; loc: Location.t}
    | Let of {id: Ident.t; exp: Exp.t; loc: Location.t}

  let pp fmt = function
    | Load {id; exp; typ} ->
        F.fprintf fmt "%a:%a = load %a" Ident.pp id Typ.pp typ Exp.pp exp
    | Store {exp1; typ; exp2} ->
        F.fprintf fmt "store %a <- %a:%a" Exp.pp exp1 Exp.pp exp2 Typ.pp typ
    | Prune {exp} ->
        F.fprintf fmt "prune %a" Exp.pp exp
    | Let {id; exp} ->
        F.fprintf fmt "%a = %a" Ident.pp id Exp.pp exp


  let of_sil decls tenv (i : Sil.instr) =
    match i with
    | Load {id; e; typ} ->
        let id = Ident.of_sil id in
        let exp = Exp.of_sil decls tenv e in
        let typ = Typ.of_sil typ in
        let loc = Location.Unknown in
        Load {id; exp; typ; loc}
    | Store {e1; typ; e2} ->
        let exp1 = Exp.of_sil decls tenv e1 in
        let typ = Typ.of_sil typ in
        let exp2 = Exp.of_sil decls tenv e2 in
        let loc = Location.Unknown in
        Store {exp1; typ; exp2; loc}
    | Prune (e, _, _, _) ->
        Prune {exp= Exp.of_sil decls tenv e; loc= Location.Unknown}
    | Call ((id, _), Const (Cfun pname), (SilExp.Sizeof {typ= {desc= Tstruct name}}, _) :: _, _, _)
      when String.equal (SilProcname.to_simplified_string pname) "__new()" ->
        let procname = TypeName.of_sil_typ_name name |> Procname.make_allocate in
        Let
          { id= Ident.of_sil id
          ; exp= Call {proc= procname.Procname.qualified_name; args= []}
          ; loc= Location.Unknown }
    | Call ((id, _), Const (Cfun pname), (SilExp.Sizeof {typ}, _) :: _, _, _)
      when String.equal (SilProcname.to_simplified_string pname) "__new_array()" ->
        let procname = Typ.of_sil typ |> Procname.make_allocate_array in
        Let
          { id= Ident.of_sil id
          ; exp= Call {proc= procname.Procname.qualified_name; args= []}
          ; loc= Location.Unknown }
    | Call ((id, _), Const (Cfun pname), args, _, _) ->
        let procname = Procname.of_sil pname in
        let () = Decls.declare_procname decls procname in
        let proc = procname.qualified_name in
        let args = List.map ~f:(fun (e, _) -> Exp.of_sil decls tenv e) args in
        let loc = Location.Unknown in
        Let {id= Ident.of_sil id; exp= Call {proc; args}; loc}
    | Call _ ->
        L.die InternalError "Translation of a SIL call that is not const not supported"
    | Metadata _ ->
        L.die InternalError "Translation of a metadata instructions not supported"


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
    | Let {exp= Call {proc; args= []}} when Procname.is_allocate_builtin proc ->
        true
    | Let {exp= Call {proc; args}} ->
        (not (Procname.is_not_regular_proc proc))
        && List.for_all args ~f:Exp.do_not_contain_regular_call
    | Let {exp= _} ->
        false


  let to_sil lang decls_env procname i : Sil.instr =
    let sourcefile = decls_env.Decls.sourcefile in
    match i with
    | Load {id; exp; typ; loc} ->
        let typ = Typ.to_sil typ in
        let id = Ident.to_sil id in
        let e = Exp.to_sil lang decls_env procname exp in
        let loc = Location.to_sil sourcefile loc in
        Load {id; e; typ; root_typ= typ; loc}
    | Store {exp1; typ; exp2; loc} ->
        let e1 = Exp.to_sil lang decls_env procname exp1 in
        let typ = Typ.to_sil typ in
        let e2 = Exp.to_sil lang decls_env procname exp2 in
        let loc = Location.to_sil sourcefile loc in
        Store {e1; root_typ= typ; typ; e2; loc}
    | Prune {exp; loc} ->
        let e = Exp.to_sil lang decls_env procname exp in
        let loc = Location.to_sil sourcefile loc in
        Prune (e, loc, true, Ik_if {terminated= false})
    | Let {id; exp= Call {proc; args= []}; loc} when Procname.is_allocate_object_builtin proc ->
        let typ = SilTyp.mk_struct (TypeName.allocate_buitin_to_java_sil proc.name) in
        let sizeof =
          SilExp.Sizeof {typ; nbytes= None; dynamic_length= None; subtype= Subtype.exact}
        in
        let class_type = SilTyp.mk_ptr typ in
        let args = [(sizeof, class_type)] in
        let ret = Ident.to_sil id in
        let loc = Location.to_sil sourcefile loc in
        let builtin_new = SilExp.Const (SilConst.Cfun BuiltinDecl.__new) in
        Call ((ret, class_type), builtin_new, args, loc, CallFlags.default)
    | Let {id; exp= Call {proc; args= [exp]}; loc} when Procname.is_allocate_array_builtin proc ->
        let element_typ = SilTyp.mk_struct TypeName.java_lang_object in
        let typ = SilTyp.mk_array element_typ in
        let e = Exp.to_sil lang decls_env procname exp in
        let sizeof =
          SilExp.Sizeof {typ; nbytes= None; dynamic_length= Some e; subtype= Subtype.exact}
        in
        let class_type = SilTyp.mk_ptr typ in
        let args = [(sizeof, class_type)] in
        let ret = Ident.to_sil id in
        let loc = Location.to_sil sourcefile loc in
        let builtin_new = SilExp.Const (SilConst.Cfun BuiltinDecl.__new_array) in
        Call ((ret, class_type), builtin_new, args, loc, CallFlags.default)
    | Let {id; exp= Call {proc; args}; loc} ->
        let ret = Ident.to_sil id in
        let procname =
          match Decls.get_procname decls_env proc with
          | Some procname ->
              procname
          | None ->
              raise
                (ToSilTransformationError
                   (fun fmt () ->
                     F.fprintf fmt "the expression in %a should start with a regular call" pp i ) )
        in
        let pname = Procname.to_sil lang procname in
        let formals_types = List.map procname.formals_types ~f:Typ.to_sil in
        let result_type = Typ.to_sil procname.result_type in
        let args = List.map ~f:(Exp.to_sil lang decls_env procname) args in
        let args =
          match List.zip args formals_types with
          | Ok l ->
              l
          | _ ->
              L.die UserError "the call %a has been given a wrong number of arguments" pp i
        in
        let loc = Location.to_sil sourcefile loc in
        let cflag = Procname.kind_to_sil_callflag procname.kind in
        Call ((ret, result_type), Const (Cfun pname), args, loc, cflag)
    | Let _ ->
        raise
          (ToSilTransformationError
             (fun fmt () ->
               F.fprintf fmt "the expression in %a should start with a regular call" pp i ) )


  let subst instr eqs =
    match instr with
    | Load args ->
        Load {args with exp= Exp.subst args.exp eqs}
    | Store args ->
        Store {args with exp1= Exp.subst args.exp1 eqs; exp2= Exp.subst args.exp2 eqs}
    | Prune args ->
        Prune {args with exp= Exp.subst args.exp eqs}
    | Let args ->
        Let {args with exp= Exp.subst args.exp eqs}
end

module Terminator = struct
  type node_call = {label: NodeName.t; ssa_args: Exp.t list}

  type t = Ret of Exp.t | Jump of node_call list | Throw of Exp.t

  let pp fmt = function
    | Ret e ->
        F.fprintf fmt "ret %a" Exp.pp e
    | Jump l ->
        let pp_block_call fmt {label; ssa_args} =
          match ssa_args with
          | [] ->
              NodeName.pp fmt label
          | _ ->
              F.fprintf fmt "%a(%a)" NodeName.pp label (pp_list_with_comma Exp.pp) ssa_args
        in
        F.fprintf fmt "jmp %a" (pp_list_with_comma pp_block_call) l
    | Throw e ->
        F.fprintf fmt "throw %a" Exp.pp e


  let do_not_contain_regular_call t =
    match t with Ret exp | Throw exp -> Exp.do_not_contain_regular_call exp | Jump _ -> true


  let to_sil lang decls_env procname pdesc loc t : Sil.instr option =
    match t with
    | Ret exp ->
        let ret_var = SilPvar.get_ret_pvar (Procname.to_sil lang procname) in
        let ret_type = Procdesc.get_ret_type pdesc in
        let e2 = Exp.to_sil lang decls_env procname exp in
        Some (Sil.Store {e1= SilExp.Lvar ret_var; root_typ= ret_type; typ= ret_type; e2; loc})
    | Jump _ ->
        None
    | Throw _ ->
        None (* TODO (T132392184) *)


  let of_sil decls tenv label_of_node ~opt_last succs =
    match opt_last with
    | None ->
        Jump (List.map ~f:(fun n -> {label= label_of_node n; ssa_args= []}) succs)
    | Some (Sil.Store {e2} as instr) when instr_is_return instr ->
        Ret (Exp.of_sil decls tenv e2)
    | Some sil_instr ->
        let pp_sil_instr fmt instr = Sil.pp_instr ~print_types:false Pp.text fmt instr in
        L.die InternalError "Unexpected instruction %a at end of block" pp_sil_instr sil_instr


  let subst t eqs =
    match t with
    | Ret exp ->
        Ret (Exp.subst exp eqs)
    | Jump node_call_list ->
        let f {label; ssa_args} =
          {label; ssa_args= List.map ssa_args ~f:(fun exp -> Exp.subst exp eqs)}
        in
        Jump (List.map node_call_list ~f)
    | Throw exp ->
        Throw (Exp.subst exp eqs)
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


  let to_sil lang decls_env procname pdesc node =
    if not (List.is_empty node.ssa_parameters) then
      raise
        (ToSilTransformationError
           (fun fmt () ->
             F.fprintf fmt "Node %a should not have SSA parameters" NodeName.pp node.label ) ) ;
    let instrs = List.map ~f:(Instr.to_sil lang decls_env procname) node.instrs in
    let last_loc = Location.to_sil decls_env.Decls.sourcefile node.last_loc in
    let last = Terminator.to_sil lang decls_env procname pdesc last_loc node.last in
    let instrs = Option.value_map ~default:instrs ~f:(fun instr -> instrs @ [instr]) last in
    let loc = Location.to_sil decls_env.Decls.sourcefile node.label_loc in
    let nkind = Procdesc.Node.Stmt_node MethodBody in
    Procdesc.create_node pdesc loc nkind instrs


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

  let of_sil decls tenv label_of_node node =
    let module Node = Procdesc.Node in
    let label = label_of_node node in
    let exn_succs = Node.get_exn node |> List.map ~f:label_of_node in
    let instrs = Node.get_instrs node |> Instrs.get_underlying_not_reversed in
    let opt_last =
      if Array.is_empty instrs then None
      else
        let last = Array.last instrs in
        if instr_is_return last then Some last else None
    in
    let rec backward_iter i acc =
      if i < 0 then acc
      else
        let instr = Instr.of_sil decls tenv instrs.(i) in
        backward_iter (i - 1) (instr :: acc)
    in
    let instrs_length = Array.length instrs in
    let instrs =
      match opt_last with
      | None ->
          backward_iter (instrs_length - 1) []
      | Some _ ->
          backward_iter (instrs_length - 2) []
    in
    let last = Terminator.of_sil decls tenv label_of_node ~opt_last (Node.get_succs node) in
    let last_loc = Node.get_last_loc node |> Location.of_sil in
    let label_loc = Node.get_loc node |> Location.of_sil in
    {label; ssa_parameters= []; exn_succs; last; instrs; last_loc; label_loc}


  let subst node eqs =
    let rev_instrs =
      List.fold node.instrs ~init:[] ~f:(fun rev_instrs (instr : Instr.t) ->
          match instr with
          | Let {id} when Ident.Map.mem id eqs ->
              rev_instrs
          | _ ->
              Instr.subst instr eqs :: rev_instrs )
    in
    let instrs = List.rev rev_instrs in
    {node with last= Terminator.subst node.last eqs; instrs}
end

module Procdesc = struct
  type t =
    { procname: Procname.t
    ; nodes: Node.t list
    ; start: NodeName.t
    ; params: VarName.t list
    ; exit_loc: Location.t }

  let is_ready_for_to_sil_conversion {nodes} =
    List.for_all nodes ~f:Node.is_ready_for_to_sil_conversion


  let build_formals {procname; params} =
    let mk_formal typ vname =
      let name = Mangled.from_string vname.VarName.value in
      let typ = Typ.to_sil typ in
      (name, typ, Annot.Item.empty)
    in
    match List.map2 procname.formals_types params ~f:mk_formal with
    | Ok l ->
        l
    | Unequal_lengths ->
        L.die InternalError "procname %a has not the same number of arg names and arg types"
          Procname.pp_qualified_name procname.qualified_name


  let build_formals_and_locals pdesc =
    let formals = build_formals pdesc in
    let make_var_data name typ : ProcAttributes.var_data =
      {name; typ; modify_in_block= false; is_constexpr= false; is_declared_unused= false}
    in
    let seen_from_formals : Mangled.Set.t =
      List.fold formals ~init:Mangled.Set.empty ~f:(fun set (name, _, _) ->
          Mangled.Set.add name set )
    in
    let _, locals =
      List.fold pdesc.nodes ~init:(seen_from_formals, []) ~f:(fun acc (node : Node.t) ->
          List.fold node.instrs ~init:acc ~f:(fun (seen, locals) (instr : Instr.t) ->
              match instr with
              | Store {exp1= Lvar var_name; typ} ->
                  let name = Mangled.from_string var_name.value in
                  if Mangled.Set.mem name seen then (seen, locals)
                  else
                    let typ = Typ.to_sil typ in
                    (Mangled.Set.add name seen, make_var_data name typ :: locals)
                  (* FIXME: check that we don't miss variables that would be inside other left
                     values like [Field] or [Index], or wait for adding locals declarations
                     (see T131910123) *)
              | Store _ | Load _ | Prune _ | Let _ ->
                  (seen, locals) ) )
    in
    (formals, locals)


  let to_sil lang decls_env cfgs ({procname; nodes; start; exit_loc} as pdesc) =
    let sourcefile = decls_env.Decls.sourcefile in
    let sil_procname = Procname.to_sil lang procname in
    let sil_ret_type = Typ.to_sil procname.result_type in
    let definition_loc = Location.to_sil sourcefile procname.qualified_name.name.loc in
    let formals, locals = build_formals_and_locals pdesc in
    let pattributes =
      { (ProcAttributes.default sourcefile sil_procname) with
        is_defined= true
      ; formals
      ; locals
      ; ret_type= sil_ret_type
      ; loc= definition_loc }
    in
    let pdesc = Cfg.create_proc_desc cfgs pattributes in
    (* Create standalone start and end nodes. Note that SIL start node does not correspond to the start node in
       Textual. The latter is more like a _first node_. *)
    let start_node = Procdesc.create_node pdesc definition_loc Procdesc.Node.Start_node [] in
    Procdesc.set_start_node pdesc start_node ;
    let exit_loc = Location.to_sil sourcefile exit_loc in
    let exit_node = Procdesc.create_node pdesc exit_loc Procdesc.Node.Exit_node [] in
    Procdesc.set_exit_node pdesc exit_node ;
    (* FIXME: special exit nodes should be added *)
    let node_map : (string, Node.t * Procdesc.Node.t) Hashtbl.t = Hashtbl.create 17 in
    List.iter nodes ~f:(fun node ->
        let data = (node, Node.to_sil lang decls_env procname pdesc node) in
        let key = node.Node.label.value in
        Hashtbl.replace node_map key data |> ignore ) ;
    ( match Hashtbl.find_opt node_map start.value with
    | Some (_, first_node) ->
        Procdesc.node_set_succs pdesc start_node ~normal:[first_node] ~exn:[]
    | None ->
        L.die InternalError "start node %a npt found" NodeName.pp start ) ;
    (* TODO: register this exit node *)
    let normal_succ : Terminator.t -> Procdesc.Node.t list = function
      | Ret _ ->
          [exit_node]
      | Jump l ->
          List.map
            ~f:(fun ({label} : Terminator.node_call) -> Hashtbl.find node_map label.value |> snd)
            l
      | Throw _ ->
          L.die InternalError "TODO: implement throw"
    in
    Hashtbl.iter
      (fun _ ((node : Node.t), sil_node) ->
        let normal = normal_succ node.last in
        let exn : Procdesc.Node.t list =
          List.map ~f:(fun name -> Hashtbl.find node_map name.NodeName.value |> snd) node.exn_succs
        in
        Procdesc.node_set_succs pdesc sil_node ~normal ~exn )
      node_map


  let pp fmt {procname; nodes; params} =
    F.fprintf fmt "@[<v 2>define %a {" (Procname.pp_with_params params) procname ;
    List.iter ~f:(F.fprintf fmt "%a" Node.pp) nodes ;
    F.fprintf fmt "@]\n}@\n@\n"


  (* returns all the idents that are defined in the procdesc *)
  let collect_ident_defs {nodes} : Ident.Set.t =
    List.fold nodes ~init:Ident.Set.empty ~f:(fun set (node : Node.t) ->
        let set =
          List.fold node.ssa_parameters ~init:set ~f:(fun set (id, _) -> Ident.Set.add id set)
        in
        List.fold node.instrs ~init:set ~f:(fun set (instr : Instr.t) ->
            match instr with
            | Load {id} | Let {id} ->
                Ident.Set.add id set
            | Store _ | Prune _ ->
                set ) )


  let subst pdesc eqs = {pdesc with nodes= List.map pdesc.nodes ~f:(fun node -> Node.subst node eqs)}

  let make_label_of_node () =
    let open Procdesc in
    let tbl = NodeHash.create 17 in
    let count = ref 0 in
    fun node ->
      match NodeHash.find_opt tbl node with
      | Some lbl ->
          lbl
      | None ->
          let value = "node_" ^ string_of_int !count in
          let res : NodeName.t = {value; loc= Location.Unknown} in
          NodeHash.add tbl node res ;
          incr count ;
          res


  let of_sil decls tenv pdesc =
    let module P = Procdesc in
    let procname = P.get_proc_name pdesc |> Procname.of_sil in
    let node_of_sil = make_label_of_node () |> Node.of_sil decls tenv in
    let start_node = P.get_start_node pdesc |> node_of_sil in
    let nodes = List.map (P.get_nodes pdesc) ~f:node_of_sil in
    let nodes = List.rev nodes in
    let nodes =
      match nodes with
      | head :: _ when Node.equal head start_node ->
          nodes
      | _ ->
          L.die InternalError "the start node is not in head"
      (* FIXME: this is fine with the current Java frontend, but we could make that more robust *)
    in
    let start = start_node.label in
    let params =
      List.map (P.get_pvar_formals pdesc) ~f:(fun (pvar, _) -> (Pvar.of_sil pvar).name)
    in
    let exit_loc = Location.Unknown in
    {procname; nodes; start; params; exit_loc}
end

module Attr = struct
  type t = {name: string; value: string; loc: Location.t}

  let name {name} = name

  let value {value} = value

  let source_language = "source_language"

  let mk_source_language value =
    {name= source_language; value= Lang.to_string value; loc= Location.Unknown}


  let pp fmt {name; value} = F.fprintf fmt "%s = \"%s\"" name value

  let pp_with_loc fmt t = F.fprintf fmt "%a: %a" Location.pp t.loc pp t
end

module SsaVerification = struct
  type error = SsaError of {id: Ident.t; locations: Location.Set.t}

  let pp_error fmt error =
    match error with
    | SsaError {id; locations} ->
        let pp_location fmt loc = F.fprintf fmt "[%a]" Location.pp loc in
        F.fprintf fmt "ident %a is defined more than once at locations %a" Ident.pp id
          (F.pp_print_list ~pp_sep:(fun fmt () -> F.pp_print_string fmt ", ") pp_location)
          (Location.Set.elements locations)


  let run (pdesc : Procdesc.t) =
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
          if Location.Set.cardinal locations > 1 then SsaError {id; locations} :: errors else errors
          )
        seen []
    in
    if not (List.is_empty errors) then
      let pp fmt () =
        F.fprintf fmt "%a"
          (F.pp_print_list ~pp_sep:(fun fmt () -> F.pp_print_string fmt "\n  ") pp_error)
          errors
      in
      raise (ToSilTransformationError pp)
end

module Module = struct
  type decl = Global of Pvar.t | Struct of Struct.t | Procname of Procname.t | Proc of Procdesc.t

  type t = {attrs: Attr.t list; decls: decl list; sourcefile: SourceFile.t}

  let lang {attrs} =
    let lang_attr =
      List.find attrs ~f:(fun (attr : Attr.t) -> String.equal attr.name Attr.source_language)
    in
    lang_attr |> Option.bind ~f:(fun x -> Attr.value x |> Lang.of_string)


  let make_decls {decls; sourcefile} =
    let decls_env = Decls.init sourcefile in
    let register decl =
      match decl with
      | Global pvar ->
          Decls.declare_global decls_env pvar
      | Struct strct ->
          Decls.declare_struct decls_env strct
      | Procname pname ->
          Decls.declare_procname decls_env pname
      | Proc pdesc ->
          Decls.declare_procname decls_env pdesc.procname
    in
    List.iter decls ~f:register ;
    decls_env


  let map_procs ~f _module =
    let decls =
      List.map _module.decls ~f:(fun decl ->
          match decl with Proc pdesc -> Proc (f pdesc) | Global _ | Struct _ | Procname _ -> decl )
    in
    {_module with decls}


  let to_sil module_ =
    match lang module_ with
    | None ->
        raise
          (ToSilTransformationError
             (fun fmt _ -> F.fprintf fmt "Missing or unsupported source_language attribute") )
    | Some lang ->
        let decls_env = make_decls module_ in
        let cfgs = Cfg.create () in
        let tenv = Tenv.create () in
        List.iter module_.decls ~f:(fun decl ->
            match decl with
            | Global _ ->
                ()
            | Struct strct ->
                Struct.to_sil lang tenv strct
            | Procname _ ->
                ()
            | Proc pdesc ->
                if not (Procdesc.is_ready_for_to_sil_conversion pdesc) then
                  (* we only run SSA verification if the to_sil conversion  needs
                     extra transformation, because some .sil files that are generated by
                     Java examples are not in SSA *)
                  SsaVerification.run pdesc ;
                Procdesc.to_sil lang decls_env cfgs pdesc ) ;
        (cfgs, tenv)


  let of_sil ~sourcefile ~lang tenv cfg =
    let env = Decls.init sourcefile in
    let decls =
      Cfg.fold_sorted cfg ~init:[] ~f:(fun decls pdesc ->
          let textual_pdesc = Procdesc.of_sil env tenv pdesc in
          Proc textual_pdesc :: decls )
    in
    let decls = Decls.fold_globals env ~init:decls ~f:(fun decls _ pvar -> Global pvar :: decls) in
    let decls =
      Decls.fold_structs env ~init:decls ~f:(fun decls _ struct_ -> Struct struct_ :: decls)
    in
    let decls =
      Decls.fold_procnames env ~init:decls ~f:(fun decls procname -> Procname procname :: decls)
    in
    let attrs = [Attr.mk_source_language lang] in
    {attrs; decls; sourcefile}


  let pp_attr fmt attr = F.fprintf fmt "attribute %a@\n@\n" Attr.pp attr

  let pp_decl fmt = function
    | Global pvar ->
        F.fprintf fmt "global %a@\n@\n" VarName.pp pvar.name
    | Proc pdesc ->
        Procdesc.pp fmt pdesc
    | Procname pname ->
        F.fprintf fmt "declare %a@\n@\n" Procname.pp pname
    | Struct struct_ ->
        F.fprintf fmt "type %a@\n@\n" Struct.pp struct_


  let pp fmt module_ =
    List.iter ~f:(pp_attr fmt) module_.attrs ;
    List.iter ~f:(pp_decl fmt) module_.decls


  let pp_copyright fmt =
    F.fprintf fmt "// \n" ;
    F.fprintf fmt "// Copyright (c) Facebook, Inc. and its affiliates.\n" ;
    F.fprintf fmt "// \n" ;
    F.fprintf fmt "// This source code is licensed under the MIT license found in the\n" ;
    F.fprintf fmt "// LICENSE file in the root directory of this source tree.\n" ;
    F.fprintf fmt "//\n\n"


  let from_java ~filename tenv cfg =
    Utils.with_file_out filename ~f:(fun oc ->
        let fmt = F.formatter_of_out_channel oc in
        let sourcefile = SourceFile.create filename in
        pp_copyright fmt ;
        pp fmt (of_sil ~sourcefile ~lang:Java tenv cfg) ;
        Format.pp_print_flush fmt () )
end

module Transformation = struct
  let remove_internal_calls _module =
    let module State = struct
      type t = {instrs_rev: Instr.t list; fresh_ident: Ident.t}

      let push_instr instr state = {state with instrs_rev= instr :: state.instrs_rev}

      let incr_fresh state = {state with fresh_ident= Ident.next state.fresh_ident}
    end in
    let rec flatten_exp (exp : Exp.t) state : Exp.t * State.t =
      match exp with
      | Var _ | Lvar _ | Const _ ->
          (exp, state)
      | Field f ->
          let exp, state = flatten_exp f.exp state in
          (Field {f with exp}, state)
      | Index (exp1, exp2) ->
          let exp1, state = flatten_exp exp1 state in
          let exp2, state = flatten_exp exp2 state in
          (Index (exp1, exp2), state)
      | Call {proc; args} ->
          let args, state = flatten_exp_list args state in
          if Procname.is_sil_instr proc then (Call {proc; args}, state)
          else
            let fresh = state.State.fresh_ident in
            let new_instr : Instr.t =
              Let {id= fresh; exp= Call {proc; args}; loc= Location.Unknown}
            in
            (Var fresh, State.push_instr new_instr state |> State.incr_fresh)
      | Cast (typ, exp) ->
          let exp, state = flatten_exp exp state in
          (Cast (typ, exp), state)
    and flatten_exp_list exp_list state =
      let exp_list, state =
        List.fold exp_list ~init:([], state) ~f:(fun (args, state) exp ->
            let exp, state = flatten_exp exp state in
            (exp :: args, state) )
      in
      (List.rev exp_list, state)
    in
    let flatten_in_instr (instr : Instr.t) state : State.t =
      match instr with
      | Load args ->
          let exp, state = flatten_exp args.exp state in
          State.push_instr (Load {args with exp}) state
      | Store args ->
          let exp1, state = flatten_exp args.exp1 state in
          let exp2, state = flatten_exp args.exp2 state in
          State.push_instr (Store {args with exp1; exp2}) state
      | Prune args ->
          let exp, state = flatten_exp args.exp state in
          State.push_instr (Prune {args with exp}) state
      | Let {id; exp= Call {proc; args}; loc} when not (Procname.is_sil_instr proc) ->
          let args, state = flatten_exp_list args state in
          State.push_instr (Let {id; exp= Call {proc; args}; loc}) state
      | Let {id; exp; loc} ->
          let exp, state = flatten_exp exp state in
          State.push_instr (Let {id; exp; loc}) state
    in
    let flatten_in_terminator (last : Terminator.t) state : Terminator.t * State.t =
      match last with
      | Ret exp ->
          let exp, state = flatten_exp exp state in
          (Ret exp, state)
      | Jump node_calls ->
          let node_calls_rev, state =
            List.fold node_calls ~init:([], state)
              ~f:(fun (node_calls, state) {Terminator.label; ssa_args} ->
                let ssa_args, state = flatten_exp_list ssa_args state in
                ({Terminator.label; ssa_args} :: node_calls, state) )
          in
          (Jump (List.rev node_calls_rev), state)
      | Throw exp ->
          let exp, state = flatten_exp exp state in
          (Throw exp, state)
    in
    let flatten_node (node : Node.t) fresh_ident : Node.t * Ident.t =
      let state =
        let init : State.t = {instrs_rev= []; fresh_ident} in
        List.fold node.instrs ~init ~f:(fun state instr -> flatten_in_instr instr state)
      in
      let last, ({instrs_rev; fresh_ident} : State.t) = flatten_in_terminator node.last state in
      ({node with last; instrs= List.rev instrs_rev}, fresh_ident)
    in
    let flatten_pdesc (pdesc : Procdesc.t) =
      let fresh = Procdesc.collect_ident_defs pdesc |> Ident.fresh in
      let _, rev_nodes =
        List.fold pdesc.nodes ~init:(fresh, []) ~f:(fun (fresh, instrs) node ->
            let node, fresh = flatten_node node fresh in
            (fresh, node :: instrs) )
      in
      {pdesc with nodes= List.rev rev_nodes}
    in
    Module.map_procs ~f:flatten_pdesc _module


  (* TODO (T131910123): replace with STORE+LOAD transform *)
  let let_propagation module_ =
    let get id ident_map =
      try Ident.Map.find id ident_map
      with Caml.Not_found ->
        L.die InternalError "Textual.let_propagation.get failed: unknown identifier %a" Ident.pp id
    in
    let build_equations pdesc : Exp.t Ident.Map.t =
      (* we collect all rule of the form [id = exp] where [exp] is not a regular call nor an
         allocation *)
      List.fold pdesc.Procdesc.nodes ~init:Ident.Map.empty ~f:(fun eqs (node : Node.t) ->
          List.fold node.instrs ~init:eqs ~f:(fun eqs (instr : Instr.t) ->
              match instr with
              | Load _ | Store _ | Prune _ ->
                  eqs
              | Let {exp= Call {proc}} when not (Procname.is_sil_instr proc) ->
                  eqs
              | Let {id; exp} ->
                  Ident.Map.add id exp eqs ) )
    in
    let compute_dependencies equations : Ident.Set.t Ident.Map.t =
      (* for each equation we record which equation it depends on for its evaluation *)
      let domain =
        Ident.Map.fold (fun id _ set -> Ident.Set.add id set) equations Ident.Set.empty
      in
      let vars exp = Ident.Set.inter (Exp.vars exp) domain in
      Ident.Map.map vars equations
    in
    let sort_equations equations dependencies : Ident.t list =
      (* returns a topological sorted list of identifiers such that if the equation of [id1] depends
          on [id2], then [id1] is after [id2] in the list.
         [dependencies] must be equal to [compute_dependencies equations] *)
      let init = (Ident.Map.empty, []) in
      let rec visit id ((status, sorted_idents) as state) =
        match Ident.Map.find_opt id status with
        | Some `VisitInProgress ->
            L.die InternalError
              "Textual transformation error: sort_equation was given a set of equations with \
               cyclic dependencies"
        | Some `VisitCompleted ->
            state
        | None ->
            let status = Ident.Map.add id `VisitInProgress status in
            let vars = get id dependencies in
            let status, sorted_idents = Ident.Set.fold visit vars (status, sorted_idents) in
            (Ident.Map.add id `VisitCompleted status, id :: sorted_idents)
      in
      let _, sorted_idents =
        Ident.Map.fold
          (fun id _ ((status, _) as state) ->
            if Ident.Map.mem id status then state else visit id state )
          equations init
      in
      List.rev sorted_idents
    in
    let transform pdesc =
      let equations = build_equations pdesc in
      let dependencies = compute_dependencies equations in
      let sorted = sort_equations equations dependencies in
      (* we saturate the equation set (id1, exp1), .. (idn, expn) by rewriting
         enough in each expi such that none depends on id1, .., idn at the end *)
      let saturated_equations =
        List.fold sorted ~init:Ident.Map.empty ~f:(fun saturated_equations id ->
            let eq = get id equations in
            let vars = get id dependencies in
            let saturated_eq =
              Ident.Set.fold
                (fun id' exp ->
                  (* thanks to the topological sort, id' has already been processed *)
                  let saturated_eq' = get id' saturated_equations in
                  Exp.subst_one exp ~id:id' ~by:saturated_eq' )
                vars eq
            in
            Ident.Map.add id saturated_eq saturated_equations )
      in
      Procdesc.subst pdesc saturated_equations
    in
    Module.map_procs ~f:transform module_


  let out_of_ssa module_ =
    let transform (pdesc : Procdesc.t) : Procdesc.t =
      let get_node : NodeName.t -> Node.t =
        let map =
          List.fold pdesc.nodes ~init:NodeName.Map.empty ~f:(fun map (node : Node.t) ->
              NodeName.Map.add node.label node map )
        in
        fun node ->
          try NodeName.Map.find node map
          with Caml.Not_found -> L.die InternalError "Textual.remove_ssa_params internal error"
      in
      let zip_ssa_args call_location (node_call : Terminator.node_call) (end_node : Node.t) :
          Instr.t list =
        match
          List.map2 end_node.ssa_parameters node_call.ssa_args ~f:(fun (id, typ) exp2 ->
              let var_name = Ident.to_ssa_var id in
              Instr.Store {exp1= Lvar var_name; typ; exp2; loc= Location.Unknown} )
        with
        | Ok equations ->
            equations
        | Unequal_lengths ->
            L.die InternalError
              "Jmp arguments at %a and block parameters at %a should have the same size" Location.pp
              call_location Location.pp end_node.label_loc
      in
      let build_assignements (start_node : Node.t) : Instr.t list =
        match (start_node.last : Terminator.t) with
        | Ret _ | Throw _ ->
            []
        | Jump node_calls ->
            List.fold node_calls ~init:[] ~f:(fun instrs (node_call : Terminator.node_call) ->
                let end_node : Node.t = get_node node_call.label in
                if List.is_empty end_node.ssa_parameters then instrs
                else
                  let let_instrs = zip_ssa_args start_node.last_loc node_call end_node in
                  List.rev_append let_instrs instrs )
      in
      let terminator_remove_args (terminator : Terminator.t) : Terminator.t =
        let node_call_remove_args (node_call : Terminator.node_call) : Terminator.node_call =
          {node_call with ssa_args= []}
        in
        match terminator with
        | Ret _ | Throw _ ->
            terminator
        | Jump node_calls ->
            Jump (List.map node_calls ~f:node_call_remove_args)
      in
      let nodes =
        List.map pdesc.nodes ~f:(fun node ->
            let rev_instrs = build_assignements node in
            let load_param (id, typ) : Instr.t =
              Load {id; exp= Lvar (Ident.to_ssa_var id); typ; loc= Location.Unknown}
            in
            let prefix = List.map node.Node.ssa_parameters ~f:load_param in
            let last = terminator_remove_args node.Node.last in
            let instrs =
              if List.is_empty rev_instrs then prefix @ node.Node.instrs
              else prefix @ node.Node.instrs @ List.rev rev_instrs
            in
            ({node with instrs; ssa_parameters= []; last} : Node.t) )
      in
      {pdesc with nodes}
    in
    Module.map_procs ~f:transform module_
end

module Verification = struct
  type error =
    | UnknownFieldname of {tname: TypeName.t; fname: FieldBaseName.t}
    | UnknownProcname of Procname.qualified_name
    | UnknownLabel of {label: NodeName.t; pname: Procname.qualified_name}
  (* TODO: check that a name is not declared twice *)
  (* TODO: add basic type verification *)

  let pp_error sourcefile fmt error =
    F.fprintf fmt "SIL consistency error in file %a" SourceFile.pp sourcefile ;
    match error with
    | UnknownFieldname {tname; fname} ->
        F.fprintf fmt ", %a: field %a.%a is not declared\n" Location.pp tname.loc TypeName.pp tname
          FieldBaseName.pp fname
    | UnknownProcname proc ->
        F.fprintf fmt ", %a: function %a is not declared\n" Location.pp proc.name.loc
          Procname.pp_qualified_name proc
    | UnknownLabel {label; pname} ->
        F.fprintf fmt ", %a: label %a is not declared in function %a\n" Location.pp label.loc
          NodeName.pp label Procname.pp_qualified_name pname


  let verify_decl ~is_fieldname_declared ~is_procname_declared errors (decl : Module.decl) =
    let verify_label errors declared_labels pname label =
      if String.Set.mem declared_labels label.NodeName.value then errors
      else UnknownLabel {label; pname} :: errors
    in
    let verify_fieldname errors tname fname =
      if is_fieldname_declared tname fname then errors
      else UnknownFieldname {tname; fname} :: errors
    in
    let verify_procname errors proc =
      if is_procname_declared proc || Procname.is_not_regular_proc proc then errors
      else UnknownProcname proc :: errors
    in
    let rec verify_exp errors (exp : Exp.t) =
      match exp with
      | Var _ | Lvar _ | Const _ ->
          errors
      | Field {exp; tname; fname} ->
          let errors = verify_fieldname errors tname fname in
          verify_exp errors exp
      | Index (e1, e2) ->
          let errors = verify_exp errors e1 in
          verify_exp errors e2
      | Call {proc; args} ->
          let errors = verify_procname errors proc in
          List.fold ~f:verify_exp ~init:errors args
      | Cast (_, e) ->
          verify_exp errors e
    in
    let verify_instr errors (instr : Instr.t) =
      match instr with
      | Load {exp} | Prune {exp} | Let {exp} ->
          verify_exp errors exp
      | Store {exp1; exp2} ->
          let errors = verify_exp errors exp1 in
          verify_exp errors exp2
    in
    let verify_procdesc errors ({procname; nodes} : Procdesc.t) =
      let declared_labels =
        List.fold nodes ~init:String.Set.empty ~f:(fun set node ->
            String.Set.add set node.Node.label.value )
      in
      let verify_label errors = verify_label errors declared_labels procname.qualified_name in
      let verify_terminator errors (t : Terminator.t) =
        match t with
        | Jump l ->
            let f errors {Terminator.label} = verify_label errors label in
            List.fold ~init:errors ~f l
        | Ret e | Throw e ->
            verify_exp errors e
      in
      let verify_node errors ({instrs; last} : Node.t) =
        let errors = List.fold ~f:verify_instr ~init:errors instrs in
        verify_terminator errors last
      in
      List.fold ~f:verify_node ~init:errors nodes
    in
    match decl with
    | Global _ | Struct _ | Procname _ ->
        errors
    | Proc pdesc ->
        verify_procdesc errors pdesc


  let run (module_ : Module.t) =
    let decls_env = Module.make_decls module_ in
    let is_fieldname_declared = Decls.is_fieldname_declared decls_env in
    let is_procname_declared = Decls.is_procname_declared decls_env in
    let f = verify_decl ~is_fieldname_declared ~is_procname_declared in
    List.fold ~f ~init:[] module_.decls
end
