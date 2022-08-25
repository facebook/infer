(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

exception ToSilTransformationError of (F.formatter -> unit -> unit)

module Location = struct
  type t = Known of {line: int; col: int} | Unknown

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
end

module type NAME = sig
  type t = {value: string; loc: Location.t}
end

(* this signature will not be exported in .mli *)
module type COMMON_NAME = sig
  include NAME

  val of_java_name : string -> t

  val equal : t -> t -> bool

  val pp : F.formatter -> t -> unit
end

module Name : COMMON_NAME = struct
  type t = {value: string; loc: Location.t}

  let replace_dot_with_2colons str = String.substr_replace_all str ~pattern:"." ~with_:"::"

  let of_java_name str = {value= replace_dot_with_2colons str; loc= Location.Unknown}

  let equal name1 name2 = String.equal name1.value name2.value

  let pp fmt name = F.pp_print_string fmt name.value
end

module ProcBaseName : COMMON_NAME = Name

module FieldBaseName : COMMON_NAME = Name

let builtin_allocate_prefix = "__sil_allocate_"

module TypeName : sig
  include COMMON_NAME

  val of_sil_typ_name : Typ.Name.t -> t

  val to_java_sil : t -> Typ.Name.t

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

module Ident = struct
  type t = int

  let to_sil id = Ident.create Ident.knormal id
  (* TODO: check the Ident generator is ready *)

  let of_int id = id

  let of_sil (id : Ident.t) =
    if not (Ident.is_normal id) then
      L.die InternalError "Textual conversion: onlyt normal ident should appear in Java"
    else Ident.get_stamp id


  let pp fmt id = F.fprintf fmt "n%d" id
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

let pp_list_with_comma pp fmt l = Pp.seq ~sep:", " pp fmt l

module SilProcname = Procname

module Procname = struct
  type kind = Virtual | NonVirtual [@@deriving equal]

  type enclosing_class = TopLevel | Enclosing of TypeName.t

  type t =
    { enclosing_class: enclosing_class
    ; name: ProcBaseName.t
    ; targs: Typ.t list
    ; tres: Typ.t
    ; kind: kind }

  let toplevel_classname = "$TOPLEVEL$CLASS$"

  let pp_enclosing_class fmt = function
    | TopLevel ->
        ()
    | Enclosing tname ->
        F.fprintf fmt "%a." TypeName.pp tname


  let pp fmt {enclosing_class; name; targs; tres} =
    F.fprintf fmt "%a%a(%a) : %a" pp_enclosing_class enclosing_class ProcBaseName.pp name
      (pp_list_with_comma Typ.pp) targs Typ.pp tres


  let pp_with_params params fmt {enclosing_class; name; targs; tres} =
    let pp fmt (typ, id) = F.fprintf fmt "%a: %a" VarName.pp id Typ.pp typ in
    match List.zip targs params with
    | Ok args ->
        F.fprintf fmt "%a%a(%a) : %a" pp_enclosing_class enclosing_class ProcBaseName.pp name
          (pp_list_with_comma pp) args Typ.pp tres
    | _ ->
        L.die InternalError "Textual printing error: params has size %d and targs has size %d"
          (List.length params) (List.length targs)


  let of_sil (pname : Procname.t) =
    match pname with
    | Java jpname ->
        let kind =
          if Procname.Java.is_static jpname then NonVirtual
          else Virtual (* FIXME: we do not handle virtuall call yet *)
        in
        let name = Procname.Java.get_method jpname |> ProcBaseName.of_java_name in
        let targs = Procname.Java.get_parameters jpname |> List.map ~f:Typ.of_sil in
        let targs =
          if Procname.Java.is_static jpname then targs
          else
            let typ = Procname.Java.get_class_type_name jpname in
            let this_type = Typ.(Ptr (Struct (TypeName.of_sil_typ_name typ))) in
            this_type :: targs
        in
        let tres = Procname.Java.get_return_typ jpname |> Typ.of_sil in
        let enclosing_class =
          Enclosing (TypeName.of_java_name (Procname.Java.get_class_name jpname))
        in
        (* FIXME when adding inheritance *)
        {enclosing_class; name; targs; tres; kind}
    | _ ->
        L.die InternalError "Non-Java procname should not appear in Java mode"


  let make_builtin ~name ~targs ~tres =
    {name; targs; tres; kind= NonVirtual; enclosing_class= TopLevel}


  let make_allocate (tname : TypeName.t) =
    let name : ProcBaseName.t =
      {value= builtin_allocate_prefix ^ tname.value; loc= Location.Unknown}
    in
    make_builtin ~name ~targs:[] ~tres:(Ptr (Struct tname))


  let unop_table : (Unop.t * string) list =
    [(Neg, "__sil_neg"); (BNot, "__sil_bnot"); (LNot, "__sil_lnot")]


  let inverse_assoc_list l = List.map l ~f:(fun (a, b) -> (b, a))

  let unop_inverse_table = inverse_assoc_list unop_table

  let of_unop unop =
    let value = List.Assoc.find_exn ~equal:Unop.equal unop_table unop in
    let name : ProcBaseName.t = {value; loc= Location.Unknown} in
    name


  let to_unop pname : Unop.t option =
    List.Assoc.find ~equal:String.equal unop_inverse_table pname.ProcBaseName.value


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
    name


  let binop_inverse_map = inverse_assoc_list binop_table |> Map.Poly.of_alist_exn

  let is_allocate_builtin (pname : ProcBaseName.t) =
    String.is_prefix ~prefix:builtin_allocate_prefix pname.value


  let is_sil_instr (pname : ProcBaseName.t) =
    let name = pname.value in
    List.Assoc.mem ~equal:String.equal unop_inverse_table name
    || Map.Poly.mem binop_inverse_map name


  let is_not_regular_proc_name (pname : ProcBaseName.t) =
    is_allocate_builtin pname || is_sil_instr pname


  let to_binop pname : Binop.t option = Map.Poly.find binop_inverse_map pname.ProcBaseName.value

  let to_sil {name; targs; tres; enclosing_class} : Procname.t =
    let class_name =
      TypeName.to_java_sil
        ( match enclosing_class with
        | TopLevel ->
            TypeName.of_java_name toplevel_classname
        | Enclosing tname ->
            tname )
    in
    let tres = Typ.to_sil tres in
    let return_type = Some tres in
    let method_name = name.ProcBaseName.value in
    let targs = List.map ~f:Typ.to_sil targs in
    let kind = Procname.Java.Non_Static (* FIXME when handling inheritance *) in
    Procname.make_java ~class_name ~return_type ~method_name ~parameters:targs ~kind


  let kind_to_sil_callflag = function
    | Virtual ->
        {CallFlags.default with cf_virtual= true}
    | _ ->
        CallFlags.default
end

let instr_is_return = function Sil.Store {e1= Lvar v} -> Pvar.is_return v | _ -> false

module Pvar = struct
  type kind = Global | Local of Procname.t

  type t = {name: VarName.t; kind: kind}

  let is_global pvar = match pvar.kind with Global -> true | _ -> false

  let to_sil {name; kind} =
    let mangled = Mangled.from_string name.value in
    match kind with
    | Global ->
        Pvar.mk_global mangled
    | Local procname ->
        let pname = Procname.to_sil procname in
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

  let to_sil tenv {name; fields; methods} =
    let name = TypeName.to_java_sil name in
    let fields =
      List.map fields ~f:(fun fname ->
          (Fieldname.to_sil fname, Typ.to_sil fname.Fieldname.typ, Annot.Item.empty) )
    in
    (* FIXME: generate static fields *)
    let methods = List.map methods ~f:Procname.to_sil in
    ignore (Tenv.mk_struct tenv ~fields ~methods name)


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
    F.fprintf fmt "%a = {@[<hov>%a2@] }" TypeName.pp name pp_fields fields
end

module Decls = struct
  (* We do not export this module. We record here each name to a more elaborate object *)

  type t =
    { globals: (string, Pvar.t) Hashtbl.t
    ; labels: (string, (string, unit) Hashtbl.t) Hashtbl.t
    ; procnames: (string, Procname.t) Hashtbl.t
    ; structs: (string, Struct.t) Hashtbl.t
    ; sourcefile: SourceFile.t }

  let init sourcefile =
    { globals= Hashtbl.create (module String)
    ; labels= Hashtbl.create (module String)
    ; procnames= Hashtbl.create (module String)
    ; structs= Hashtbl.create (module String)
    ; sourcefile }


  let declare_global decls (pvar : Pvar.t) =
    let key = pvar.name.value in
    ignore (Hashtbl.add decls.globals ~key ~data:pvar)


  let declare_label decls pname label =
    let tbl =
      match Hashtbl.find decls.labels pname.ProcBaseName.value with
      | Some tbl ->
          tbl
      | None ->
          let tbl = Hashtbl.create (module String) in
          ignore (Hashtbl.add decls.labels ~key:pname.ProcBaseName.value ~data:tbl) ;
          tbl
    in
    ignore (Hashtbl.add tbl ~key:label.NodeName.value ~data:())


  let declare_procname decls (pname : Procname.t) =
    ignore (Hashtbl.add decls.procnames ~key:pname.name.value ~data:pname)


  let declare_struct decls (s : Struct.t) =
    ignore (Hashtbl.add decls.structs ~key:s.name.value ~data:s)


  let declare_struct_from_tenv decls tenv (tname : TypeName.t) =
    match Hashtbl.find decls.structs tname.value with
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
    match Hashtbl.find decls.structs tname.value with
    | None ->
        false
    | Some struct_ ->
        List.exists struct_.fields ~f:(fun {Fieldname.name} -> FieldBaseName.equal name fname)


  let is_label_declared decls pname label =
    match Hashtbl.find decls.labels pname.ProcBaseName.value with
    | None ->
        false
    | Some tbl ->
        Hashtbl.mem tbl label.NodeName.value


  let is_procname_declared decls pname = Hashtbl.mem decls.procnames pname.ProcBaseName.value

  let get_global decls (vname : VarName.t) = Hashtbl.find decls.globals vname.value

  let get_fieldname decls (tname : TypeName.t) (fname : FieldBaseName.t) =
    let open IOption.Let_syntax in
    let* strct = Hashtbl.find decls.structs tname.value in
    List.find strct.Struct.fields ~f:(fun {Fieldname.name} -> FieldBaseName.equal name fname)


  let get_procname decls (pname : ProcBaseName.t) = Hashtbl.find decls.procnames pname.value

  let fold_globals decls ~init ~f =
    Hashtbl.fold ~init ~f:(fun ~key ~data x -> f x key data) decls.globals


  let fold_procnames decls ~init ~f =
    Hashtbl.fold ~init ~f:(fun ~key ~data x -> f x key data) decls.procnames


  let fold_structs decls ~init ~f =
    Hashtbl.fold ~init ~f:(fun ~key ~data x -> f x key data) decls.structs
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
    | Call of {proc: ProcBaseName.t; args: t list}
    | Cast of Typ.t * t

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
        ProcBaseName.pp fmt proc ;
        pp_list fmt args
    | Cast (typ, e) ->
        F.fprintf fmt "(%a %a)" Typ.pp typ pp e


  and pp_list fmt l = F.fprintf fmt "(%a)" (pp_list_with_comma pp) l

  let to_sil decls_env procname exp =
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
          Lvar (Pvar.to_sil pvar)
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
              ProcBaseName.pp proc
            (* FIXME: transform instruction to put call at head of expressions *) )
      | Cast (typ, exp) ->
          Cast (Typ.to_sil typ, aux exp)
    in
    aux exp
end

module Instr = struct
  type t =
    | Load of {id: Ident.t; exp: Exp.t; typ: Typ.t; loc: Location.t}
    | Store of {exp1: Exp.t; typ: Typ.t; exp2: Exp.t; loc: Location.t}
    | Prune of {exp: Exp.t; b: bool; loc: Location.t}
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
    | Prune (e, _, b, _) ->
        Prune {exp= Exp.of_sil decls tenv e; b; loc= Location.Unknown}
    | Call ((id, _), Const (Cfun pname), (SilExp.Sizeof {typ= {desc= Tstruct name}}, _) :: _, _, _)
      when String.equal (SilProcname.to_simplified_string pname) "__new()" ->
        let procname = TypeName.of_sil_typ_name name |> Procname.make_allocate in
        let () = Decls.declare_procname decls procname in
        Let
          { id= Ident.of_sil id
          ; exp= Call {proc= procname.Procname.name; args= []}
          ; loc= Location.Unknown }
    | Call ((id, _), Const (Cfun pname), args, _, _) ->
        let procname = Procname.of_sil pname in
        let () = Decls.declare_procname decls procname in
        let proc = procname.name in
        let args = List.map ~f:(fun (e, _) -> Exp.of_sil decls tenv e) args in
        let loc = Location.Unknown in
        Let {id= Ident.of_sil id; exp= Call {proc; args}; loc}
    | Call _ ->
        L.die InternalError "Translation of a SIL call that is not const not supported"
    | Metadata _ ->
        L.die InternalError "Translation of a metadata instructions not supported"


  let to_sil decls_env procname i : Sil.instr =
    let sourcefile = decls_env.Decls.sourcefile in
    match i with
    | Load {id; exp; typ; loc} ->
        let typ = Typ.to_sil typ in
        let id = Ident.to_sil id in
        let e = Exp.to_sil decls_env procname exp in
        let loc = Location.to_sil sourcefile loc in
        Load {id; e; typ; root_typ= typ; loc}
    | Store {exp1; typ; exp2; loc} ->
        let e1 = Exp.to_sil decls_env procname exp1 in
        let typ = Typ.to_sil typ in
        let e2 = Exp.to_sil decls_env procname exp2 in
        let loc = Location.to_sil sourcefile loc in
        Store {e1; root_typ= typ; typ; e2; loc}
    | Prune {exp; b; loc} ->
        let e = Exp.to_sil decls_env procname exp in
        let loc = Location.to_sil sourcefile loc in
        Prune (e, loc, b, Ik_if {terminated= false})
    | Let {id; exp= Call {proc; args= []}; loc} when Procname.is_allocate_builtin proc ->
        let typ = SilTyp.mk_struct (TypeName.allocate_buitin_to_java_sil proc) in
        let sizeof =
          SilExp.Sizeof {typ; nbytes= None; dynamic_length= None; subtype= Subtype.exact}
        in
        let class_type = SilTyp.mk_ptr typ in
        let args = [(sizeof, class_type)] in
        let ret = Ident.to_sil id in
        let loc = Location.to_sil sourcefile loc in
        let builtin_new = SilExp.Const (SilConst.Cfun BuiltinDecl.__new) in
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
        let pname = Procname.to_sil procname in
        let targs = List.map procname.targs ~f:Typ.to_sil in
        let tres = Typ.to_sil procname.tres in
        let args = List.map ~f:(Exp.to_sil decls_env procname) args in
        let args =
          match List.zip args targs with
          | Ok l ->
              l
          | _ ->
              L.die UserError "the call %a has been given a wrong number of arguments" pp i
        in
        let loc = Location.to_sil sourcefile loc in
        let cflag = Procname.kind_to_sil_callflag procname.kind in
        Call ((ret, tres), Const (Cfun pname), args, loc, cflag)
    | Let _ ->
        raise
          (ToSilTransformationError
             (fun fmt () ->
               F.fprintf fmt "the expression in %a should start with a regular call" pp i ) )
end

module Terminator = struct
  type node_call = {label: NodeName.t; ssa_args: Ident.t list}

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
              F.fprintf fmt "%a(%a)" NodeName.pp label (pp_list_with_comma Ident.pp) ssa_args
        in
        F.fprintf fmt "jmp %a" (pp_list_with_comma pp_block_call) l
    | Throw e ->
        F.fprintf fmt "throw %a" Exp.pp e


  let of_sil decls tenv label_of_node ~opt_last succs =
    match opt_last with
    | None ->
        Jump (List.map ~f:(fun n -> {label= label_of_node n; ssa_args= []}) succs)
    | Some (Sil.Store {e2} as instr) when instr_is_return instr ->
        Ret (Exp.of_sil decls tenv e2)
    | Some sil_instr ->
        let pp_sil_instr fmt instr = Sil.pp_instr ~print_types:false Pp.text fmt instr in
        L.die InternalError "Unexpected instruction %a at end of block" pp_sil_instr sil_instr
end

module Node = struct
  type t =
    { label: NodeName.t
    ; ssa_parameters: Ident.t list
    ; exn_succs: NodeName.t list
    ; last: Terminator.t
    ; instrs: Instr.t list
    ; last_loc: Location.t
    ; label_loc: Location.t }

  let to_sil decls_env procname pdesc node =
    if not (List.is_empty node.ssa_parameters) then
      raise
        (ToSilTransformationError
           (fun fmt () ->
             F.fprintf fmt "Node %a should not have SSA parameters" NodeName.pp node.label ) ) ;
    let instrs = List.map ~f:(Instr.to_sil decls_env procname) node.instrs in
    let loc = Location.to_sil decls_env.Decls.sourcefile node.label_loc in
    let nkind = Procdesc.Node.Stmt_node MethodBody in
    Procdesc.create_node pdesc loc nkind instrs


  let pp fmt node =
    F.fprintf fmt "@\n@[<v 4>#%a:" NodeName.pp node.label ;
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
end

module Procdesc = struct
  type t =
    { procname: Procname.t
    ; nodes: Node.t list
    ; start: NodeName.t
    ; params: VarName.t list
    ; exit_loc: Location.t }

  let to_sil decls_env cfgs {procname; nodes; start; params; exit_loc} =
    let sourcefile = decls_env.Decls.sourcefile in
    let sil_procname = Procname.to_sil procname in
    let pattributes = ProcAttributes.default sourcefile sil_procname in
    let locals =
      match
        List.map2 procname.targs params ~f:(fun typ vname ->
            let name = Mangled.from_string vname.VarName.value in
            let typ = Typ.to_sil typ in
            ProcAttributes.
              {name; typ; modify_in_block= false; is_constexpr= false; is_declared_unused= false} )
      with
      | Ok l ->
          l
      | Unequal_lengths ->
          L.die InternalError "procname %a has not the same number of arg names and arg types"
            ProcBaseName.pp procname.name
    in
    let () = pattributes.locals <- locals in
    let pdesc = Cfg.create_proc_desc cfgs pattributes in
    (* FIXME: special exit nodes should be added *)
    let node_map : (string, Node.t * Procdesc.Node.t) Hashtbl.t = Hashtbl.create (module String) in
    List.iter nodes ~f:(fun node ->
        let data = (node, Node.to_sil decls_env procname pdesc node) in
        let key = node.Node.label.value in
        ignore (Hashtbl.add node_map ~key ~data) ) ;
    ( match Hashtbl.find node_map start.value with
    | Some (_, start_node) ->
        Procdesc.set_start_node pdesc start_node
    | None ->
        L.die InternalError "start node %a npt found" NodeName.pp start ) ;
    let exit_loc = Location.to_sil sourcefile exit_loc in
    let exit_node = Procdesc.create_node pdesc exit_loc Procdesc.Node.Exit_node [] in
    (* TODO: register this exit node *)
    let normal_succ : Terminator.t -> Procdesc.Node.t list = function
      | Ret _ ->
          [exit_node]
      (* FIXME: generate a ret assignment *)
      | Jump l ->
          List.map
            ~f:(fun ({label} : Terminator.node_call) -> Hashtbl.find_exn node_map label.value |> snd)
            l
      | Throw _ ->
          L.die InternalError "TODO: implement throw"
    in
    Hashtbl.iter node_map ~f:(fun ((node : Node.t), sil_node) ->
        let normal = normal_succ node.last in
        let exn : Procdesc.Node.t list =
          List.map
            ~f:(fun name -> Hashtbl.find_exn node_map name.NodeName.value |> snd)
            node.exn_succs
        in
        Procdesc.node_set_succs pdesc sil_node ~normal ~exn )


  let pp fmt {procname; nodes; params} =
    F.fprintf fmt "@[<v 2>define %a {" (Procname.pp_with_params params) procname ;
    List.iter ~f:(F.fprintf fmt "%a" Node.pp) nodes ;
    F.fprintf fmt "@]\n}@\n@\n"


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

module Module = struct
  type decl = Global of Pvar.t | Struct of Struct.t | Procname of Procname.t | Proc of Procdesc.t

  type t = {decls: decl list; sourcefile: SourceFile.t}

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
          let register_label node =
            let label = node.Node.label in
            let pname = pdesc.procname.name in
            Decls.declare_label decls_env pname label
          in
          List.iter pdesc.nodes ~f:register_label ;
          Decls.declare_procname decls_env pdesc.procname
    in
    List.iter decls ~f:register ;
    decls_env


  let to_sil module_ =
    let decls_env = make_decls module_ in
    let cfgs = Cfg.create () in
    let tenv = Tenv.create () in
    List.iter module_.decls ~f:(fun decl ->
        match decl with
        | Global _ ->
            ()
        | Struct strct ->
            Struct.to_sil tenv strct
        | Procname _ ->
            ()
        | Proc pdesc ->
            Procdesc.to_sil decls_env cfgs pdesc ) ;
    (cfgs, tenv)


  let of_sil ~sourcefile tenv cfg =
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
      Decls.fold_procnames env ~init:decls ~f:(fun decls _ procname -> Procname procname :: decls)
    in
    {decls; sourcefile}


  let pp_decl fmt = function
    | Global pvar ->
        F.fprintf fmt "global %a@\n@\n" VarName.pp pvar.name
    | Proc pdesc ->
        Procdesc.pp fmt pdesc
    | Procname pname ->
        F.fprintf fmt "declare %a@\n@\n" Procname.pp pname
    | Struct struct_ ->
        F.fprintf fmt "type %a@\n@\n" Struct.pp struct_


  let pp fmt module_ = List.iter ~f:(pp_decl fmt) module_.decls

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
        pp fmt (of_sil ~sourcefile tenv cfg) ;
        Format.pp_print_flush fmt () )
end

module Verification = struct
  type error =
    | UnknownFieldname of {tname: TypeName.t; fname: FieldBaseName.t}
    | UnknownProcname of {pname: ProcBaseName.t}
    | UnknownLabel of {label: NodeName.t; pname: ProcBaseName.t}
  (* TODO: check that a name is not declared twice *)
  (* TODO: add basic type verification *)

  let pp_error sourcefile fmt error =
    F.fprintf fmt "SIL consistency error in file %a" SourceFile.pp sourcefile ;
    match error with
    | UnknownFieldname {tname; fname} ->
        F.fprintf fmt ", %a: field %a.%a is not declared\n" Location.pp tname.loc TypeName.pp tname
          FieldBaseName.pp fname
    | UnknownProcname {pname} ->
        F.fprintf fmt ", %a: function %a is not declared\n" Location.pp pname.loc ProcBaseName.pp
          pname
    | UnknownLabel {label; pname} ->
        F.fprintf fmt ", %a: label %a is not declared in function %a\n" Location.pp label.loc
          NodeName.pp label ProcBaseName.pp pname


  let verify_decl ~is_label_declared ~is_fieldname_declared ~is_procname_declared errors
      (decl : Module.decl) =
    let verify_label errors pname label =
      if is_label_declared pname label then errors else UnknownLabel {label; pname} :: errors
    in
    let verify_fieldname errors tname fname =
      if is_fieldname_declared tname fname then errors
      else UnknownFieldname {tname; fname} :: errors
    in
    let verify_procname errors pname =
      if is_procname_declared pname || Procname.is_not_regular_proc_name pname then errors
      else UnknownProcname {pname} :: errors
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
      let verify_label errors = verify_label errors procname.name in
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
    let is_label_declared = Decls.is_label_declared decls_env in
    let is_fieldname_declared = Decls.is_fieldname_declared decls_env in
    let is_procname_declared = Decls.is_procname_declared decls_env in
    let f = verify_decl ~is_label_declared ~is_fieldname_declared ~is_procname_declared in
    List.fold ~f ~init:[] module_.decls
end
