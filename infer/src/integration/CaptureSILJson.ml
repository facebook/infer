(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Capture module for SIL in JSON format *)

open! IStd
open Yojson.Safe.Util
(* We use [Yojson.Safe] to parse json so it handles long integers, which cannot be handled by
   OCaml's basic integers *)

module L = Logging
module Hashtbl = Caml.Hashtbl
module Safe = Yojson.Safe

module IntHash = struct
  type t = int

  let equal i j = Int.equal i j

  let hash i = i land Int.max_value
end

module IntTbl = Hashtbl.Make (IntHash)

exception JsonParse_Error of string

let typename_of_classname cn = Typ.Name.CSharp.from_string cn

let parse_list (eleparse : Safe.t -> 'a) (json : Safe.t) = List.map ~f:eleparse (to_list json)

let parse_parameter (str : string) : Annot.parameter = Annot.{name= Some str; value= Annot.Str str}

let parse_list_parameters (eleparse : Safe.t -> 'a) (json : Safe.t) =
  List.map ~f:eleparse (to_list json) |> List.map ~f:parse_parameter


let parse_cil_type_name =
  let r = Str.regexp "\\." in
  fun (str : string) : Typ.t ->
    try
      let n = Str.search_backward r str (String.length str) in
      let _namespace = Str.string_before str n in
      let _name = Str.string_after str (n + 1) in
      Typ.(
        mk_ptr
          (mk_struct
             (CSharpClass (CSharpClassName.make ~namespace:(Some _namespace) ~classname:_name)) ) )
    with _ ->
      Typ.(mk_ptr (mk_struct (CSharpClass (CSharpClassName.make ~namespace:None ~classname:str))))


let parse_cil_procname (json : Safe.t) : Procname.t =
  let method_name = to_string (member "method_name" json) in
  match method_name with
  | "__new" ->
      BuiltinDecl.__new
  | "__new_array" ->
      BuiltinDecl.__new_array
  | "__set_locked_attribute" ->
      BuiltinDecl.__set_locked_attribute
  | "__delete_locked_attribute" ->
      BuiltinDecl.__delete_locked_attribute
  | "__instanceof" ->
      BuiltinDecl.__instanceof
  | "__cast" ->
      BuiltinDecl.__cast
  | "__unwrap_exception" ->
      BuiltinDecl.__unwrap_exception
  | "__throw" ->
      BuiltinDecl.__java_throw
  | "__get_array_length" ->
      BuiltinDecl.__get_array_length
  | _ ->
      let return_type =
        if String.equal Procname.CSharp.constructor_method_name method_name then None
        else Some (to_string (member "return_type" json) |> parse_cil_type_name)
      in
      let class_name = to_string (member "class_name" json) |> Typ.Name.CSharp.from_string in
      let param_types = parse_list to_string (member "parameters" json) in
      let params = List.map ~f:parse_cil_type_name param_types in
      let is_static = to_bool (member "is_static" json) in
      let method_kind = if is_static then Procname.CSharp.Static else Procname.CSharp.Non_Static in
      Procname.make_csharp ~class_name ~return_type ~method_name ~parameters:params
        ~kind:method_kind


let parse_ikind (json : Safe.t) =
  let ikind_map =
    [ ("IChar", Typ.IChar)
    ; ("ISChar", Typ.ISChar)
    ; ("IUChar", Typ.IUChar)
    ; ("IBool", Typ.IBool)
    ; ("IInt", Typ.IInt)
    ; ("IUInt", Typ.IUInt)
    ; ("IShort", Typ.IShort)
    ; ("IUShort", Typ.IUShort)
    ; ("ILong", Typ.ILong)
    ; ("IULong", Typ.IULong)
    ; ("ILongLong", Typ.ILongLong)
    ; ("IULongLong", Typ.IULongLong)
    ; ("I128", Typ.I128)
    ; ("IU128", Typ.IU128) ]
  in
  List.Assoc.find_exn ~equal:String.equal ikind_map (to_string json)


let parse_fkind (json : Safe.t) =
  let fkind_map =
    [("FFloat", Typ.FFloat); ("FDouble", Typ.FDouble); ("FLongDouble", Typ.FLongDouble)]
  in
  List.Assoc.find_exn ~equal:String.equal fkind_map (to_string json)


let parse_ptr_kind (json : Safe.t) =
  let ptr_kind_map =
    [ ("Pk_pointer", Typ.Pk_pointer)
    ; ("Pk_lvalue_reference", Typ.Pk_lvalue_reference)
    ; ("Pk_rvalue_reference", Typ.Pk_rvalue_reference)
    ; ("Pk_objc_weak", Typ.Pk_objc_weak)
    ; ("Pk_objc_unsafe_unretained", Typ.Pk_objc_unsafe_unretained)
    ; ("Pk_objc_autoreleasing", Typ.Pk_objc_autoreleasing) ]
  in
  List.Assoc.find_exn ~equal:String.equal ptr_kind_map (to_string json)


let parse_if_kind (json : Safe.t) =
  let ifkind_map =
    [ ("Ik_bexp", Sil.Ik_bexp {terminated= false})
    ; ("Ik_dowhile", Sil.Ik_dowhile)
    ; ("Ik_for", Sil.Ik_for)
    ; ("Ik_if", Sil.Ik_if {terminated= false})
    ; ("Ik_land_lor", Sil.Ik_land_lor)
    ; ("Ik_while", Sil.Ik_while)
    ; ("Ik_switch", Sil.Ik_switch) ]
  in
  List.Assoc.find_exn ~equal:String.equal ifkind_map (to_string json)


let parse_csu (json : Safe.t) =
  let csu = to_string (member "csu_kind" json) in
  let name = to_string (member "name" json) in
  match csu with
  | "Class" ->
      typename_of_classname name
  | _ ->
      raise (JsonParse_Error "JSON Parse Error: Can only parse Class types so far.")


let parse_unop (json : Safe.t) =
  let unop_map = [("Neg", Unop.Neg); ("BNot", Unop.BNot); ("LNot", Unop.LNot)] in
  List.Assoc.find_exn ~equal:String.equal unop_map (to_string json)


let parse_binop (json : Safe.t) =
  (*TODO: need to check the usage of "None" *)
  let binop_map =
    [ ("PlusA", Binop.PlusA None)
    ; ("PlusPI", Binop.PlusPI)
    ; ("MinusA", Binop.MinusA None)
    ; ("MinusPI", Binop.MinusPI)
    ; ("MinusPP", Binop.MinusPP)
    ; ("Mult", Binop.Mult None)
    ; ("Div", Binop.DivI) (* for backwards compatibility *)
    ; ("DivI", Binop.DivI)
    ; ("DivF", Binop.DivF)
    ; ("Mod", Binop.Mod)
    ; ("Shiftlt", Binop.Shiftlt)
    ; ("Shiftrt", Binop.Shiftrt)
    ; ("Lt", Binop.Lt)
    ; ("Gt", Binop.Gt)
    ; ("Le", Binop.Le)
    ; ("Ge", Binop.Ge)
    ; ("Eq", Binop.Eq)
    ; ("Ne", Binop.Ne)
    ; ("BAnd", Binop.BAnd)
    ; ("BXor", Binop.BXor)
    ; ("BOr", Binop.BOr)
    ; ("LAnd", Binop.LAnd)
    ; ("LOr", Binop.LOr) ]
  in
  List.Assoc.find_exn ~equal:String.equal binop_map (to_string json)


let parse_typename (json : Safe.t) =
  let tname = to_string (member "type_name_kind" json) in
  if String.equal tname "TN_typedef" then typename_of_classname (to_string (member "name" json))
  else if String.equal tname "CsuTypeName" then parse_csu json
    (*what about if the name is <Module>*)
  else Logging.die InternalError "Can't parse typename"


let parse_long (json : Safe.t) = Int64.of_string (Yojson.Safe.to_string json)

let parse_intrep (json : Safe.t) =
  let v = parse_long (member "value" json) in
  let p = to_bool (member "is_pointer" json) in
  match (p, v) with true, 0L -> IntLit.null | _ -> IntLit.of_int64 v


let parse_floatrep (json : Safe.t) = Float.of_string (Yojson.Safe.to_string json)

let parse_ident (json : Safe.t) =
  let k = to_string (member "kind" json) in
  let kind =
    if String.equal k "Normal" then Ident.knormal
    else if String.equal k "Primed" then Ident.kprimed
    else if String.equal k "Footprint" then Ident.kfootprint
    else if String.equal k "None" then Ident.knone
    else Logging.die InternalError "Unsupported identifier kind: %s" k
  in
  Ident.create_with_stamp kind
    (Ident.string_to_name (to_string (member "name" json)))
    (to_int (member "stamp" json))


let parse_fieldident (json : Safe.t) =
  Fieldname.make StdTyp.Name.CSharp.system_string (to_string (member "field_name" json))


let parse_source_file (json : Safe.t) =
  let p = to_string (member "path" json) in
  SourceFile.create ~check_abs_path:false p


let parse_location (json : Safe.t) =
  { Location.line= to_int (member "line" json)
  ; Location.col= to_int (member "col" json)
  ; Location.file= parse_source_file (member "source_file" json)
  ; macro_file_opt= None
  ; macro_line= -1 }


let rec parse_pvar (json : Safe.t) =
  let pvname = Mangled.from_string (to_string (member "pv_name" json)) in
  let pvkind = to_string (member "pv_kind" json) in
  if String.equal pvkind "LocalVariable" then
    let pname = parse_cil_procname (member "proc_name" json) in
    Pvar.mk pvname pname
  else if String.equal pvkind "CalledVariable" then
    let pname = parse_cil_procname (member "proc_name" json) in
    Pvar.mk_callee pvname pname
  else if String.equal pvkind "GlobalVariable" then Pvar.mk_global pvname
  else Logging.die InternalError "Unknown program variable kind %s" pvkind


and parse_constant (json : Safe.t) =
  let const_kind = to_string (member "kind" json) in
  let const_value = member "const_value" json in
  if String.equal const_kind "Int" then
    let i = parse_intrep const_value in
    Const.Cint i
  else if String.equal const_kind "Float" then
    try
      let f = parse_floatrep const_value in
      Const.Cfloat f
    with _ -> Const.Cfloat Float.nan
  else if String.equal const_kind "Fun" then
    let pname = parse_cil_procname const_value in
    Const.Cfun pname
  else if String.equal const_kind "Str" then Const.Cstr (to_string const_value)
  else if String.equal const_kind "Class" then
    Const.Cclass (Ident.string_to_name (to_string const_value))
  else Logging.die InternalError "Unknown constant kind %s" const_kind


and parse_exp (json : Safe.t) =
  let ekind = to_string (member "expr_kind" json) in
  if String.equal ekind "VarExpression" then Exp.Var (parse_ident (member "identifier" json))
  else if String.equal ekind "UnopExpression" then
    let op = parse_unop (member "operator" json) in
    let e = parse_exp (member "expression" json) in
    let t =
      let t_nullable = member "type" json in
      match t_nullable with `Null -> None | _ -> Some (parse_sil_type_name t_nullable)
    in
    Exp.UnOp (op, e, t)
  else if String.equal ekind "BinopExpression" then
    let op = parse_binop (member "operator" json) in
    let e1 = parse_exp (member "left" json) in
    let e2 = parse_exp (member "right" json) in
    Exp.BinOp (op, e1, e2)
  else if String.equal ekind "ExnExpression" then
    let e = parse_exp (member "expression" json) in
    Exp.Exn e
  else if String.equal ekind "ConstExpression" then Exp.Const (parse_constant json)
  else if String.equal ekind "CastExpression" then
    let t = parse_sil_type_name (member "type" json) in
    let e = parse_exp (member "expression" json) in
    Exp.Cast (t, e)
  else if String.equal ekind "LvarExpression" then Exp.Lvar (parse_pvar (member "pvar" json))
  else if String.equal ekind "LfieldExpression" then
    let e = parse_exp (member "expression" json) in
    let fi = parse_fieldident (member "identifier" json) in
    let t = parse_sil_type_name (member "type" json) in
    Exp.Lfield (e, fi, t)
  else if String.equal ekind "LindexExpression" then
    let e1 = parse_exp (member "array" json) in
    let e2 = parse_exp (member "index" json) in
    Exp.Lindex (e1, e2)
  else if String.equal ekind "SizeofExpression" then
    let t = parse_sil_type_name (member "type" json) in
    let s = to_string (member "kind" json) in
    let dl = try Some (parse_exp (member "dynamic_length" json)) with Type_error _ -> None in
    match s with
    | "exact" ->
        Exp.Sizeof
          {typ= t; nbytes= None; dynamic_length= dl; subtype= Subtype.exact; nullable= false}
    | "instof" ->
        Exp.Sizeof
          { typ= t
          ; nbytes= None
          ; dynamic_length= dl
          ; subtype= Subtype.subtypes_instof
          ; nullable= false }
    | "cast" ->
        Exp.Sizeof
          {typ= t; nbytes= None; dynamic_length= dl; subtype= Subtype.subtypes_cast; nullable= false}
    | _ ->
        Logging.die InternalError "Subtype in Sizeof instruction is not supported."
  else Logging.die InternalError "Unknown expression kind %s" ekind


and parse_struct_field (json : Safe.t) =
  let fi = parse_fieldident json in
  let t = parse_sil_type_name (member "type" json) in
  let annot = parse_item_annotation (member "annotation" json) in
  Struct.mk_field fi t ~annot


and parse_sil_type_name (json : Safe.t) : Typ.t =
  let type_kind = to_string (member "type_kind" json) in
  if String.equal type_kind "Tarray" then
    let t = parse_sil_type_name (member "content_type" json) in
    Typ.mk_array t
  else if String.equal type_kind "Tfloat" then
    let fkind = parse_fkind (member "kind" json) in
    Typ.mk (Typ.Tfloat fkind)
  else if String.equal type_kind "Tint" then
    let ikind = parse_ikind (member "kind" json) in
    Typ.mk (Typ.Tint ikind)
  else if String.equal type_kind "Tptr" then
    let t = parse_sil_type_name (member "type" json) in
    let pkind = parse_ptr_kind (member "kind" json) in
    Typ.mk (Typ.Tptr (t, pkind))
  else if String.equal type_kind "Tstruct" then
    let tn = typename_of_classname (to_string (member "struct_name" json)) in
    Typ.mk (Tstruct tn)
  else if String.equal type_kind "Tvar" then
    let tn = parse_typename (member "type_name" json) in
    Typ.mk (Typ.TVar (Typ.Name.name tn))
  else if String.equal type_kind "Tvoid" then StdTyp.void
  else if String.equal type_kind "Tfun" then Typ.mk Tfun
  else if String.equal type_kind "Tenum" then
    (* Sil.Tenum (parse_list (parse_pair (fun n -> Mangled.from_string (to_string n)) parse_constant) value) *)
    Logging.die InternalError "Enums are not supported yet"
  else Logging.die InternalError "Unknown sil type kind %s" type_kind


and parse_item_annotation (json : Safe.t) : Annot.Item.t =
  let parse_annotation (json : Safe.t) =
    let class_name = to_string (member "class_name" json) in
    let p = member "params" json in
    let parameters = parse_list_parameters to_string p in
    {Annot.class_name; Annot.parameters}
  in
  parse_list
    (fun j ->
      let a = member "annotation" j in
      parse_annotation a )
    (member "annotations" json)


and parse_struct (json : Safe.t) =
  let fields = parse_list parse_struct_field (member "instance_fields" json) in
  let statics = parse_list parse_struct_field (member "static_fields" json) in
  let supers = parse_list parse_csu (member "supers" json) in
  let methods = parse_list parse_cil_procname (member "methods" json) in
  let annots = parse_item_annotation json in
  (fields, statics, supers, methods, annots)


let parse_ret_annot (json : Safe.t) : Annot.Item.t =
  parse_item_annotation (member "return_value" json)


let parse_captured_var (json : Safe.t) =
  let pvar = parse_pvar (member "name" json) in
  let typ = parse_sil_type_name (member "type" json) in
  {CapturedVar.pvar; typ; capture_mode= ByValue}


let parse_proc_attributes_var (json : Safe.t) =
  let n = to_string (member "name" json) in
  let t = parse_sil_type_name (member "type" json) in
  (Mangled.from_string n, t, CapturedVar.ByValue)


let parse_proc_attributes_formals (json : Safe.t) =
  let n, t, _ = parse_proc_attributes_var json in
  (n, t)


let parse_proc_attributes_locals (json : Safe.t) : ProcAttributes.var_data =
  let n, t, _ = parse_proc_attributes_var json in
  let mib = to_bool (member "modify_in_block" json) in
  let ice = to_bool (member "is_const_expr" json) in
  { name= n
  ; typ= t
  ; modify_in_block= mib
  ; is_constexpr= ice
  ; is_declared_unused= false
  ; is_structured_binding= false
  ; has_cleanup_attribute= false
  ; tmp_id= None }


let parse_proc_attributes (json : Safe.t) =
  let access : ProcAttributes.access =
    match to_string (member "access" json) with
    | "Default" ->
        Default
    | "Public" ->
        Public
    | "Private" ->
        Private
    | "Protected" ->
        Protected
    | atype ->
        L.die InternalError "Unsupported access type %s" atype
  in
  let captured = parse_list parse_captured_var (member "captured" json) in
  let formals =
    let mangled_typs = parse_list parse_proc_attributes_formals (member "formals" json) in
    let annots =
      let json = member "method_annotations" json in
      parse_list parse_item_annotation (member "params" json)
    in
    let rec construct_formals mangled_typs annots =
      match (mangled_typs, annots) with
      | (mangled, typ) :: mangled_typs, annot :: annots ->
          (mangled, typ, annot) :: construct_formals mangled_typs annots
      | _, [] ->
          List.map mangled_typs ~f:(fun (mangled, typ) -> (mangled, typ, Annot.Item.empty))
      | [], _ ->
          []
    in
    construct_formals mangled_typs annots
  in
  let locals = parse_list parse_proc_attributes_locals (member "locals" json) in
  let loc = parse_location (member "loc" json) in
  let file = loc.file in
  let proc_name = parse_cil_procname (member "proc_name" json) in
  { (ProcAttributes.default file proc_name) with
    access
  ; captured
  ; exceptions= parse_list to_string (member "exceptions" json)
  ; formals
  ; is_abstract= to_bool (member "is_abstract" json)
  ; is_bridge_method= to_bool (member "is_bridge_method" json)
  ; is_defined= to_bool (member "is_defined" json)
  ; is_synthetic_method= to_bool (member "is_synthetic_method" json)
  ; loc
  ; locals
  ; ret_type= parse_sil_type_name (member "ret_type" json)
  ; ret_annots= parse_ret_annot (member "method_annotations" json) }


let parse_call_flags (json : Safe.t) =
  { CallFlags.default with
    CallFlags.cf_virtual= to_bool (member "cf_virtual" json)
  ; CallFlags.cf_is_objc_block= to_bool (member "cf_is_objc_block" json) }


let parse_call_args (json : Safe.t) =
  let e = parse_exp (member "expression" json) in
  let t = parse_sil_type_name (member "type" json) in
  (e, t)


let parse_instr (json : Safe.t) =
  let instr_kind = to_string (member "instruction_kind" json) in
  let l = parse_location (member "location" json) in
  if String.equal instr_kind "Load" then
    let i = parse_ident (member "identifier" json) in
    let e = parse_exp (member "expression" json) in
    let t = parse_sil_type_name (member "type" json) in
    Sil.Load {id= i; e; typ= t; loc= l}
  else if String.equal instr_kind "Store" then
    let e1 = parse_exp (member "lvalue" json) in
    let e2 = parse_exp (member "rvalue" json) in
    let t = parse_sil_type_name (member "type" json) in
    Sil.Store {e1; typ= t; e2; loc= l}
  else if String.equal instr_kind "Prune" then
    let e = parse_exp (member "condition" json) in
    let f = to_bool (member "true_branch" json) in
    let k = parse_if_kind (member "if_kind" json) in
    Sil.Prune (e, l, f, k)
  else if String.equal instr_kind "Call" then
    let rs =
      (parse_ident (member "return_var" json), parse_sil_type_name (member "return_type" json))
    in
    let e = parse_exp (member "function_expression" json) in
    let ps = parse_list parse_call_args (member "args" json) in
    let fs = parse_call_flags (member "flags" json) in
    Sil.Call (rs, e, ps, l, fs)
  else Logging.die InternalError "Unknown instruction kind %s" instr_kind


(* This has the side-effect of inserting the procedure description into the CFG. *)
let parse_pdesc (cfg : Cfg.t) (pd_id_to_pd : Procdesc.t IntTbl.t) (start_nd_tbl : int IntTbl.t)
    (exit_nd_tbl : int IntTbl.t) (json : Safe.t) =
  let _attrs = parse_proc_attributes (member "pd_attributes" json) in
  let _id = to_int (member "pd_id" json) in
  (* Store away start/end node, to be filled in later *)
  IntTbl.add start_nd_tbl _id (to_int (member "pd_start_node" json)) ;
  IntTbl.add exit_nd_tbl _id (to_int (member "pd_exit_node" json)) ;
  (* let open Procdesc in *)
  let pd =
    let pname = _attrs.proc_name in
    match Procname.Hash.find_opt cfg pname with
    | Some pdesc ->
        pdesc
    | None ->
        Cfg.create_proc_desc cfg _attrs
  in
  IntTbl.add pd_id_to_pd _id pd


(* Expect the entire node json to be passed *)
let parse_stmt_nodekind (json : Safe.t) : Procdesc.Node.stmt_nodekind =
  let nk_comment = member "stmt_node_comment" json in
  match to_string (member "stmt_node_kind" json) with
  | "AssertionFailure" ->
      Procdesc.Node.AssertionFailure
  | "BetweenJoinAndExit" ->
      Procdesc.Node.BetweenJoinAndExit
  | "BinaryConditionalStmtInit" ->
      Procdesc.Node.BinaryConditionalStmtInit
  | "BinaryOperatorStmt" ->
      Procdesc.Node.BinaryOperatorStmt (to_string nk_comment)
  | "Call" ->
      Procdesc.Node.Call (to_string nk_comment)
  | "CallObjCNew" ->
      Procdesc.Node.CallObjCNew
  | "ClassCastException" ->
      Procdesc.Node.ClassCastException
  | "ConditionalStmtBranch" ->
      Procdesc.Node.ConditionalStmtBranch
  | "ConstructorInit" ->
      Procdesc.Node.ConstructorInit
  | "CXXDynamicCast" ->
      Procdesc.Node.CXXDynamicCast
  | "CXXNewExpr" ->
      Procdesc.Node.CXXNewExpr
  | "CXXStdInitializerListExpr" ->
      Procdesc.Node.CXXStdInitializerListExpr
  | "CXXTypeidExpr" ->
      Procdesc.Node.CXXTypeidExpr
  | "DeclStmt" ->
      Procdesc.Node.DeclStmt
  | "DefineBody" ->
      Procdesc.Node.DefineBody
  | "ExceptionHandler" ->
      Procdesc.Node.ExceptionHandler
  | "ExceptionsSink" ->
      Procdesc.Node.ExceptionsSink
  | "FinallyBranch" ->
      Procdesc.Node.FinallyBranch
  | "GCCAsmStmt" ->
      Procdesc.Node.GCCAsmStmt
  | "GenericSelectionExpr" ->
      Procdesc.Node.GenericSelectionExpr
  | "IfStmtBranch" ->
      Procdesc.Node.IfStmtBranch
  | "InitializeDynamicArrayLength" ->
      Procdesc.Node.InitializeDynamicArrayLength
  | "InitListExp" ->
      Procdesc.Node.InitListExp
  | "MessageCall" ->
      Procdesc.Node.MessageCall (to_string nk_comment)
  | "MethodBody" ->
      Procdesc.Node.MethodBody
  | "MonitorEnter" ->
      Procdesc.Node.MonitorEnter
  | "MonitorExit" ->
      Procdesc.Node.MonitorExit
  | "ObjCCPPThrow" ->
      Procdesc.Node.ObjCCPPThrow
  | "OutOfBound" ->
      Procdesc.Node.OutOfBound
  | "ReturnStmt" ->
      Procdesc.Node.ReturnStmt
  | "Skip" ->
      Procdesc.Node.Skip
  | "SwitchStmt" ->
      Procdesc.Node.SwitchStmt
  | "ThisNotNull" ->
      Procdesc.Node.ThisNotNull
  | "Throw" ->
      Procdesc.Node.Throw
  | "ThrowNPE" ->
      Procdesc.Node.ThrowNPE
  | "UnaryOperator" ->
      Procdesc.Node.UnaryOperator
  | snk ->
      Logging.die InternalError "Unknown stmt node kind %s" snk


let parse_prune_nodekind (json : Safe.t) : Procdesc.Node.prune_node_kind =
  match to_string json with
  | "ExceptionHandler" ->
      PruneNodeKind_ExceptionHandler
  | "FalseBranch" ->
      PruneNodeKind_FalseBranch
  | "InBound" ->
      PruneNodeKind_InBound
  | "IsInstance" ->
      PruneNodeKind_IsInstance
  | "MethodBody" ->
      PruneNodeKind_MethodBody
  | "NotNull" ->
      PruneNodeKind_NotNull
  | "TrueBranch" ->
      PruneNodeKind_TrueBranch
  | pnk ->
      Logging.die InternalError "Unknown prune node kind %s" pnk


let parse_nodekind (_pd_id_to_pd : Procdesc.t IntTbl.t) (json : Safe.t) =
  let nkname = to_string (member "nd_kind" json) in
  if String.equal nkname "StartNode" then Procdesc.Node.Start_node
  else if String.equal nkname "ExitNode" then Procdesc.Node.Exit_node
  else if String.equal nkname "StatementNode" then
    Procdesc.Node.Stmt_node (parse_stmt_nodekind json)
  else if String.equal nkname "JoinNode" then Procdesc.Node.Join_node
  else if String.equal nkname "PruneNode" then
    let f = to_bool (member "true_branch" json) in
    let ik = parse_if_kind (member "if_kind" json) in
    let d = parse_prune_nodekind (member "prune_node_kind" json) in
    Procdesc.Node.Prune_node (f, ik, d)
  else if String.equal nkname "SkipNode" then
    Procdesc.Node.Skip_node (to_string (member "skip_node_comment" json))
  else Logging.die InternalError "Unknown nodekind: %s" nkname


let parse_node (pd_id_to_pd : Procdesc.t IntTbl.t) (nd_id_to_node : Procdesc.Node.t IntTbl.t)
    (nd_id_to_exn_nodes : int list IntTbl.t) (nd_id_to_pred_nodes : int list IntTbl.t)
    (nd_id_to_succ_nodes : int list IntTbl.t) (json : Safe.t) =
  let nd_id = to_int (member "nd_id" json) in
  IntTbl.add nd_id_to_exn_nodes nd_id (parse_list to_int (member "nd_exn_ids" json)) ;
  let nd_instrs = parse_list parse_instr (member "nd_instrs" json) in
  let nd_kind = parse_nodekind pd_id_to_pd json in
  let nd_loc = parse_location (member "nd_loc" json) in
  IntTbl.add nd_id_to_pred_nodes nd_id (parse_list to_int (member "nd_pred_ids" json)) ;
  IntTbl.add nd_id_to_succ_nodes nd_id (parse_list to_int (member "nd_succ_ids" json)) ;
  let nd_proc_desc = IntTbl.find pd_id_to_pd (to_int (member "nd_proc_id" json)) in
  let node = Procdesc.create_node nd_proc_desc nd_loc nd_kind nd_instrs in
  IntTbl.add nd_id_to_node nd_id node ;
  node


let parse_cfg (json : Safe.t) =
  let cfg = Cfg.create () in
  (* These hold information that's in the procedure description or nodes, but can only be completed once we've parsed all nodes. *)
  let pd_id_to_pd = IntTbl.create 1000 in
  let pd_id_to_start_node = IntTbl.create 1000 in
  let pd_id_to_exit_node = IntTbl.create 1000 in
  let nd_id_to_node = IntTbl.create 1000 in
  let nd_id_to_exn_nodes = IntTbl.create 1000 in
  let nd_id_to_pred_nodes = IntTbl.create 1000 in
  let nd_id_to_succ_nodes = IntTbl.create 1000 in
  List.iter
    ~f:(fun (_, pdjson) -> parse_pdesc cfg pd_id_to_pd pd_id_to_start_node pd_id_to_exit_node pdjson)
    (to_assoc (member "procs" json)) ;
  parse_list
    (parse_node pd_id_to_pd nd_id_to_node nd_id_to_exn_nodes nd_id_to_pred_nodes nd_id_to_succ_nodes)
    (member "nodes" json)
  |> ignore ;
  (* Now fix up the dangling ends *)
  IntTbl.iter
    (fun pd_id pd ->
      let start_node = IntTbl.find nd_id_to_node (IntTbl.find pd_id_to_start_node pd_id) in
      let exit_node = IntTbl.find nd_id_to_node (IntTbl.find pd_id_to_exit_node pd_id) in
      Procdesc.set_start_node pd start_node ;
      Procdesc.set_exit_node pd exit_node )
    pd_id_to_pd ;
  IntTbl.iter
    (fun (nd_id : int) (node : Procdesc.Node.t) ->
      let exn_nodes =
        List.map ~f:(IntTbl.find nd_id_to_node) (IntTbl.find nd_id_to_exn_nodes nd_id)
      in
      let succ_nodes =
        List.map ~f:(IntTbl.find nd_id_to_node) (IntTbl.find nd_id_to_succ_nodes nd_id)
      in
      Procdesc.set_succs node ~normal:(Some succ_nodes) ~exn:(Some exn_nodes) )
    nd_id_to_node ;
  cfg


let add_proc_to_cfg source_file pname pdesc map =
  let cfg = SourceFile.Map.find_opt source_file map |> IOption.if_none_eval ~f:Cfg.create in
  Procname.Hash.add cfg pname pdesc ;
  SourceFile.Map.add source_file cfg map


let store cfg =
  let save_proc procname proc_desc cfg_map =
    let attributes = Procdesc.get_attributes proc_desc in
    let source_file = attributes.loc.Location.file in
    add_proc_to_cfg source_file procname proc_desc cfg_map
  in
  let cfg_map = Procname.Hash.fold save_proc cfg SourceFile.Map.empty in
  SourceFile.Map.iter (fun sf mapped_cfg -> SourceFiles.add sf mapped_cfg Tenv.Global None) cfg_map


let parse_tenv_type (json : Safe.t) tenv =
  let tn = parse_typename (member "type_name" json) in
  let fields, statics, supers, methods, annots = parse_struct (member "type_struct" json) in
  ignore (Tenv.mk_struct tenv ~fields ~statics ~methods ~supers ~annots tn)


let parse_tenv (json : Safe.t) =
  let tenv = Tenv.create () in
  List.iter ~f:(fun entry -> parse_tenv_type entry tenv) (to_list json) ;
  tenv


let capture ~cfg_json ~tenv_json =
  let tenv = parse_tenv (Yojson.Safe.from_file tenv_json) in
  let cfg = parse_cfg (Yojson.Safe.from_file cfg_json) in
  Tenv.store_global ~normalize:true tenv ;
  store cfg ;
  ()
