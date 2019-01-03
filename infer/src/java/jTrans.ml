(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Javalib_pack
open Sawja_pack
module L = Logging

type invoke_kind = I_Virtual | I_Interface | I_Special | I_Static

exception Frontend_error of string

(** Fix the line associated to a method definition.
    Since Sawja often reports a method off by a few lines, we search
    backwards for a line where the method name is. *)
let fix_method_definition_line linereader proc_name loc =
  let proc_name_java = match proc_name with Typ.Procname.Java p -> p | _ -> assert false in
  let method_name =
    if Typ.Procname.is_constructor proc_name then
      let inner_class_name cname =
        match String.rsplit2 cname ~on:'$' with Some (_, icn) -> icn | None -> cname
      in
      inner_class_name (Typ.Procname.Java.get_simple_class_name proc_name_java)
    else Typ.Procname.Java.get_method proc_name_java
  in
  let regex = Str.regexp (Str.quote method_name) in
  let method_is_defined_here linenum =
    match Printer.LineReader.from_file_linenum_original linereader loc.Location.file linenum with
    | None ->
        raise Caml.Not_found
    | Some line -> (
      try
        ignore (Str.search_forward regex line 0) ;
        true
      with Caml.Not_found -> false )
  in
  let line = ref loc.Location.line in
  try
    while not (method_is_defined_here !line) do
      line := !line - 1 ;
      if !line < 0 then raise Caml.Not_found
    done ;
    {loc with Location.line= !line}
  with Caml.Not_found -> loc


let get_location source_file impl pc =
  let line_number =
    let ln = try JBir.get_source_line_number pc impl with Invalid_argument _ -> None in
    match ln with None -> 0 | Some n -> n
  in
  {Location.line= line_number; col= -1; file= source_file}


let get_start_location source_file bytecode =
  let line_number = Option.value (JCode.get_source_line_number 0 bytecode) ~default:(-1) in
  {Location.line= line_number; col= -1; file= source_file}


let get_exit_location source_file bytecode =
  let last_line_number =
    let compare (_, ln1) (_, ln2) = Int.compare ln1 ln2 in
    Option.value_map ~default:(-1)
      ~f:(fun l -> Option.value_map ~f:snd ~default:(-1) (List.max_elt ~compare l))
      bytecode.JCode.c_line_number_table
  in
  {Location.line= last_line_number; col= -1; file= source_file}


let retrieve_fieldname fieldname =
  let subs = Str.split (Str.regexp (Str.quote ".")) (Typ.Fieldname.to_string fieldname) in
  List.last_exn subs


let get_field_name program static tenv cn fs =
  let {Typ.Struct.fields; statics} = JTransType.get_class_struct_typ program tenv cn in
  match
    List.find
      ~f:(fun (fieldname, _, _) -> String.equal (retrieve_fieldname fieldname) (JBasics.fs_name fs))
      (if static then statics else fields)
  with
  | Some (fieldname, _, _) ->
      fieldname
  | None ->
      (* TODO (T28155039): understand why fields cannot be found here *)
      JTransType.create_fieldname cn fs


let formals_from_signature program tenv cn ms kind =
  let counter = ref 0 in
  let method_name = JBasics.ms_name ms in
  let get_arg_name () =
    let arg = method_name ^ "_arg_" ^ string_of_int !counter in
    incr counter ; Mangled.from_string arg
  in
  let collect l vt =
    let arg_name = get_arg_name () in
    let arg_type = JTransType.value_type program tenv vt in
    (arg_name, arg_type) :: l
  in
  let init_arg_list =
    match kind with
    | Typ.Procname.Java.Static ->
        []
    | Typ.Procname.Java.Non_Static ->
        [(JConfig.this, JTransType.get_class_type program tenv cn)]
  in
  List.rev (List.fold ~f:collect ~init:init_arg_list (JBasics.ms_args ms))


(** Creates the list of formal variables from a procedure based on ... *)
let translate_formals program tenv cn impl =
  let collect l (vt, var) =
    let name = Mangled.from_string (JBir.var_name_g var) in
    let typ = JTransType.param_type program tenv cn var vt in
    (name, typ) :: l
  in
  List.rev (List.fold ~f:collect ~init:[] (JBir.params impl))


(** Creates the list of local variables from the bytecode and add the variables from
    the JBir representation *)
let translate_locals program tenv formals bytecode jbir_code =
  let formal_set =
    List.fold ~f:(fun set (var, _) -> Mangled.Set.add var set) ~init:Mangled.Set.empty formals
  in
  let collect (seen_vars, l) (var, typ) =
    if Mangled.Set.mem var seen_vars then (seen_vars, l)
    else (Mangled.Set.add var seen_vars, (var, typ) :: l)
  in
  let with_bytecode_vars =
    (* Do not consider parameters as local variables *)
    let init = (formal_set, []) in
    match bytecode.JCode.c_local_variable_table with
    | None ->
        init
    | Some variable_table ->
        List.fold
          ~f:(fun accu (_, _, var_name, var_type, _) ->
            let var = Mangled.from_string var_name
            and typ = JTransType.value_type program tenv var_type in
            collect accu (var, typ) )
          ~init variable_table
  in
  (* TODO (#4040807): Needs to add the JBir temporary variables since other parts of the
     code are still relying on those *)
  let with_jbir_vars =
    Array.fold
      ~f:(fun accu jbir_var ->
        let var = Mangled.from_string (JBir.var_name_g jbir_var) in
        collect accu (var, Typ.mk Tvoid) )
      ~init:with_bytecode_vars (JBir.vars jbir_code)
  in
  snd with_jbir_vars


let get_constant (c : JBir.const) =
  match c with
  | `Int i ->
      Const.Cint (IntLit.of_int32 i)
  | `ANull ->
      Const.Cint IntLit.null
  | `Class ot ->
      Const.Cclass (Ident.string_to_name (JTransType.object_type_to_string ot))
  | `Double f ->
      Const.Cfloat f
  | `Float f ->
      Const.Cfloat f
  | `Long i64 ->
      Const.Cint (IntLit.of_int64 i64)
  | `String jstr ->
      Const.Cstr (JBasics.jstr_pp jstr)


let get_binop typ binop =
  match binop with
  | JBir.Add _ ->
      Binop.PlusA (Typ.get_ikind_opt typ)
  | JBir.Sub _ ->
      Binop.MinusA (Typ.get_ikind_opt typ)
  | JBir.Mult _ ->
      Binop.Mult (Typ.get_ikind_opt typ)
  | JBir.Div _ ->
      Binop.Div
  | JBir.Rem _ ->
      Binop.Mod
  | JBir.IAnd ->
      Binop.BAnd
  | JBir.IShl ->
      Binop.Shiftlt
  | JBir.IShr ->
      Binop.Shiftrt
  | JBir.IOr ->
      Binop.BOr
  | JBir.IXor ->
      Binop.BXor
  | JBir.IUshr ->
      Binop.Shiftrt
  | JBir.LShl ->
      Binop.Shiftlt
  | JBir.LShr ->
      Binop.Shiftrt
  | JBir.LAnd ->
      Binop.BAnd
  | JBir.LOr ->
      Binop.BOr
  | JBir.LXor ->
      Binop.BXor
  | JBir.LUshr ->
      Binop.Shiftrt
  | JBir.CMP _ ->
      raise (Frontend_error "Comparison operators")
  | JBir.ArrayLoad _ ->
      raise (Frontend_error "Array load operator")


let get_test_operator op =
  match op with
  | `Eq ->
      Binop.Eq
  | `Ge ->
      Binop.Ge
  | `Gt ->
      Binop.Gt
  | `Le ->
      Binop.Le
  | `Lt ->
      Binop.Lt
  | `Ne ->
      Binop.Ne


let is_java_native cm = Poly.( = ) cm.Javalib.cm_implementation Javalib.Native

let is_clone ms = String.equal (JBasics.ms_name ms) JConfig.clone_name

let get_bytecode cm =
  match cm.Javalib.cm_implementation with
  | Javalib.Native ->
      let cms = cm.Javalib.cm_class_method_signature in
      let cn, ms = JBasics.cms_split cms in
      L.(die InternalError)
        "native method %s found in %s@." (JBasics.ms_name ms) (JBasics.cn_name cn)
  | Javalib.Java t ->
      (* Sawja doesn't handle invokedynamic, and it will crash with a Match_failure if we give it
         bytecode with this instruction. hack around this problem by converting all invokedynamic's
         to invokestatic's that call a method with the same signature as the lambda on
         java.lang.Object. this isn't great, but it's a lot better than crashing *)
      let bytecode = Lazy.force t in
      let c_code =
        Array.map
          ~f:(function
            | JCode.OpInvoke (`Dynamic _, ms) ->
                JCode.OpInvoke (`Static JBasics.java_lang_object, ms)
            | opcode ->
                opcode)
          bytecode.JCode.c_code
      in
      {bytecode with JCode.c_code}


let get_jbir_representation cm bytecode =
  JBir.transform ~bcv:false ~ch_link:false ~formula:false ~formula_cmd:[] ~almost_ssa:true cm
    bytecode


let trans_access = function
  | `Default ->
      PredSymb.Default
  | `Public ->
      PredSymb.Public
  | `Private ->
      PredSymb.Private
  | `Protected ->
      PredSymb.Protected


let create_callee_attributes tenv program cn ms procname =
  let f jclass =
    try
      let jmethod = Javalib.get_method jclass ms in
      let formals =
        formals_from_signature program tenv cn ms (JTransType.get_method_kind jmethod)
      in
      let ret_type = JTransType.return_type program tenv ms in
      let access, method_annotation, exceptions, is_abstract =
        match jmethod with
        | Javalib.AbstractMethod am ->
            ( trans_access am.Javalib.am_access
            , JAnnotation.translate_method am.Javalib.am_annotations
            , List.map ~f:JBasics.cn_name am.Javalib.am_exceptions
            , true )
        | Javalib.ConcreteMethod cm ->
            ( trans_access cm.Javalib.cm_access
            , JAnnotation.translate_method cm.Javalib.cm_annotations
            , List.map ~f:JBasics.cn_name cm.Javalib.cm_exceptions
            , false )
      in
      (* getting the correct path to the source is cumbersome to do here, and nothing uses this data
         yet so ignore this issue *)
      let translation_unit = SourceFile.invalid __FILE__ in
      Some
        { (ProcAttributes.default translation_unit procname) with
          ProcAttributes.access; exceptions; method_annotation; formals; ret_type; is_abstract }
    with Caml.Not_found -> None
  in
  Option.bind ~f (JClasspath.lookup_node cn program)


let create_empty_cfg source_file procdesc =
  let location = Location.none source_file in
  let start_node = Procdesc.create_node procdesc location Procdesc.Node.Start_node [] in
  let exit_node = Procdesc.create_node procdesc location Procdesc.Node.Exit_node [] in
  Procdesc.node_set_succs_exn procdesc start_node [exit_node] [exit_node] ;
  Procdesc.set_start_node procdesc start_node ;
  Procdesc.set_exit_node procdesc exit_node ;
  procdesc


let create_am_procdesc source_file program icfg am proc_name : Procdesc.t =
  let tenv = icfg.JContext.tenv in
  let m = Javalib.AbstractMethod am in
  let cn, ms = JBasics.cms_split (Javalib.get_class_method_signature m) in
  let formals = formals_from_signature program tenv cn ms (JTransType.get_method_kind m) in
  let method_annotation = JAnnotation.translate_method am.Javalib.am_annotations in
  let procdesc =
    let proc_attributes =
      { (ProcAttributes.default source_file proc_name) with
        ProcAttributes.access= trans_access am.Javalib.am_access
      ; exceptions= List.map ~f:JBasics.cn_name am.Javalib.am_exceptions
      ; formals
      ; is_abstract= true
      ; is_bridge_method= am.Javalib.am_bridge
      ; is_model= Config.models_mode
      ; is_synthetic_method= am.Javalib.am_synthetic
      ; method_annotation
      ; ret_type= JTransType.return_type program tenv ms
      ; loc= Location.none source_file }
    in
    Cfg.create_proc_desc icfg.JContext.cfg proc_attributes
  in
  create_empty_cfg source_file procdesc


let create_native_procdesc source_file program icfg cm proc_name =
  let tenv = icfg.JContext.tenv in
  let m = Javalib.ConcreteMethod cm in
  let cn, ms = JBasics.cms_split (Javalib.get_class_method_signature m) in
  let formals = formals_from_signature program tenv cn ms (JTransType.get_method_kind m) in
  let method_annotation = JAnnotation.translate_method cm.Javalib.cm_annotations in
  let procdesc =
    let proc_attributes =
      { (ProcAttributes.default source_file proc_name) with
        ProcAttributes.access= trans_access cm.Javalib.cm_access
      ; exceptions= List.map ~f:JBasics.cn_name cm.Javalib.cm_exceptions
      ; formals
      ; is_bridge_method= cm.Javalib.cm_bridge
      ; is_model= Config.models_mode
      ; is_synthetic_method= cm.Javalib.cm_synthetic
      ; method_annotation
      ; ret_type= JTransType.return_type program tenv ms
      ; loc= Location.none source_file }
    in
    Cfg.create_proc_desc icfg.JContext.cfg proc_attributes
  in
  create_empty_cfg source_file procdesc


let create_empty_procdesc source_file program linereader icfg cm proc_name =
  let tenv = icfg.JContext.tenv in
  let m = Javalib.ConcreteMethod cm in
  let cn, ms = JBasics.cms_split (Javalib.get_class_method_signature m) in
  let bytecode = get_bytecode cm in
  let loc_start =
    get_start_location source_file bytecode |> fix_method_definition_line linereader proc_name
  in
  let formals = formals_from_signature program tenv cn ms (JTransType.get_method_kind m) in
  let method_annotation = JAnnotation.translate_method cm.Javalib.cm_annotations in
  let proc_attributes =
    { (ProcAttributes.default source_file proc_name) with
      ProcAttributes.access= trans_access cm.Javalib.cm_access
    ; exceptions= List.map ~f:JBasics.cn_name cm.Javalib.cm_exceptions
    ; formals
    ; is_bridge_method= cm.Javalib.cm_bridge
    ; is_model= Config.models_mode
    ; is_synthetic_method= cm.Javalib.cm_synthetic
    ; is_java_synchronized_method= cm.Javalib.cm_synchronized
    ; loc= loc_start
    ; method_annotation
    ; ret_type= JTransType.return_type program tenv ms }
  in
  let proc_desc = Cfg.create_proc_desc icfg.JContext.cfg proc_attributes in
  create_empty_cfg source_file proc_desc


(** Creates a procedure description. *)
let create_cm_procdesc source_file program linereader icfg cm proc_name =
  let cfg = icfg.JContext.cfg in
  let tenv = icfg.JContext.tenv in
  let m = Javalib.ConcreteMethod cm in
  let cn, ms = JBasics.cms_split (Javalib.get_class_method_signature m) in
  try
    let bytecode = get_bytecode cm in
    let jbir_code = get_jbir_representation cm bytecode in
    let loc_start =
      get_start_location source_file bytecode |> fix_method_definition_line linereader proc_name
    in
    let loc_exit = get_exit_location source_file bytecode in
    let formals = translate_formals program tenv cn jbir_code in
    let locals_ = translate_locals program tenv formals bytecode jbir_code in
    let locals =
      List.map locals_ ~f:(fun (name, typ) ->
          ({name; typ; modify_in_block= false; is_constexpr= false} : ProcAttributes.var_data) )
    in
    let method_annotation = JAnnotation.translate_method cm.Javalib.cm_annotations in
    let proc_attributes =
      { (ProcAttributes.default source_file proc_name) with
        ProcAttributes.access= trans_access cm.Javalib.cm_access
      ; exceptions= List.map ~f:JBasics.cn_name cm.Javalib.cm_exceptions
      ; formals
      ; is_bridge_method= cm.Javalib.cm_bridge
      ; is_defined= true
      ; is_model= Config.models_mode
      ; is_synthetic_method= cm.Javalib.cm_synthetic
      ; is_java_synchronized_method= cm.Javalib.cm_synchronized
      ; loc= loc_start
      ; locals
      ; method_annotation
      ; ret_type= JTransType.return_type program tenv ms }
    in
    let procdesc = Cfg.create_proc_desc cfg proc_attributes in
    let start_node = Procdesc.create_node procdesc loc_start Procdesc.Node.Start_node [] in
    let exit_node = Procdesc.create_node procdesc loc_exit Procdesc.Node.Exit_node [] in
    let exn_kind = Procdesc.Node.exn_sink_kind in
    let exn_node = Procdesc.create_node procdesc loc_exit exn_kind [] in
    JContext.add_exn_node proc_name exn_node ;
    Procdesc.set_start_node procdesc start_node ;
    Procdesc.set_exit_node procdesc exit_node ;
    Some (procdesc, start_node, exit_node, exn_node, jbir_code)
  with JBir.Subroutine ->
    L.internal_error "create_procdesc raised JBir.Subroutine when translating %a in %a@."
      Typ.Procname.pp proc_name SourceFile.pp source_file ;
    None


let builtin_new = Exp.Const (Const.Cfun BuiltinDecl.__new)

let builtin_get_array_length = Exp.Const (Const.Cfun BuiltinDecl.__get_array_length)

let create_sil_deref exp typ loc =
  let no_id = Ident.create_none () in
  Sil.Load (no_id, exp, typ, loc)


(** translate an expression used as an r-value *)
let rec expression (context : JContext.t) pc expr =
  let program = context.program in
  let loc = get_location context.source_file context.impl pc in
  let tenv = JContext.get_tenv context in
  let type_of_expr = JTransType.expr_type context expr in
  let trans_var pvar =
    let id = Ident.create_fresh Ident.knormal in
    let sil_instr = Sil.Load (id, Exp.Lvar pvar, type_of_expr, loc) in
    ([sil_instr], Exp.Var id, type_of_expr)
  in
  match expr with
  | JBir.Var (_, var) ->
      let pvar = JContext.set_pvar context var type_of_expr in
      trans_var pvar
  | JBir.Const c -> (
    match c with
    (* We use the constant <field> internally to mean a variable. *)
    | `String s when String.equal (JBasics.jstr_pp s) JConfig.field_cst ->
        let varname = JConfig.field_st in
        let procname = Procdesc.get_proc_name context.procdesc in
        let pvar = Pvar.mk varname procname in
        trans_var pvar
    | _ ->
        ([], Exp.Const (get_constant c), type_of_expr) )
  | JBir.Unop (unop, ex) -> (
      let type_of_ex = JTransType.expr_type context ex in
      let instrs, sil_ex, _ = expression context pc ex in
      match unop with
      | JBir.Neg _ ->
          (instrs, Exp.UnOp (Unop.Neg, sil_ex, Some type_of_expr), type_of_expr)
      | JBir.ArrayLength ->
          let array_typ_no_ptr =
            match type_of_ex.Typ.desc with Typ.Tptr (typ, _) -> typ | _ -> type_of_ex
          in
          let deref = create_sil_deref sil_ex array_typ_no_ptr loc in
          let args = [(sil_ex, type_of_ex)] in
          let ret_id = Ident.create_fresh Ident.knormal in
          let ret_typ = Typ.mk (Tint IInt) in
          let call_instr =
            Sil.Call ((ret_id, ret_typ), builtin_get_array_length, args, loc, CallFlags.default)
          in
          (instrs @ [deref; call_instr], Exp.Var ret_id, type_of_expr)
      | JBir.Conv conv ->
          let cast_ex = Exp.Cast (JTransType.cast_type conv, sil_ex) in
          (instrs, cast_ex, type_of_expr)
      | JBir.InstanceOf ot | JBir.Cast ot ->
          let subtypes =
            match unop with
            | JBir.InstanceOf _ ->
                Subtype.subtypes_instof
            | JBir.Cast _ ->
                Subtype.subtypes_cast
            | _ ->
                assert false
          in
          let sizeof_expr = JTransType.sizeof_of_object_type program tenv ot subtypes in
          let builtin =
            match unop with
            | JBir.InstanceOf _ ->
                Exp.Const (Const.Cfun BuiltinDecl.__instanceof)
            | JBir.Cast _ ->
                Exp.Const (Const.Cfun BuiltinDecl.__cast)
            | _ ->
                assert false
          in
          let args = [(sil_ex, type_of_ex); (sizeof_expr, Typ.mk Tvoid)] in
          let ret_id = Ident.create_fresh Ident.knormal in
          let call =
            Sil.Call ((ret_id, Typ.mk (Tint IBool)), builtin, args, loc, CallFlags.default)
          in
          let res_ex = Exp.Var ret_id in
          (instrs @ [call], res_ex, type_of_expr) )
  | JBir.Binop (binop, ex1, ex2) -> (
      let instrs1, sil_ex1, _ = expression context pc ex1
      and instrs2, sil_ex2, _ = expression context pc ex2 in
      match binop with
      | JBir.ArrayLoad _ ->
          (* add an instruction that dereferences the array *)
          let array_typ = Typ.mk_array type_of_expr in
          let deref_array_instr = create_sil_deref sil_ex1 array_typ loc in
          let id = Ident.create_fresh Ident.knormal in
          let load_instr = Sil.Load (id, Exp.Lindex (sil_ex1, sil_ex2), type_of_expr, loc) in
          let instrs = (instrs1 @ (deref_array_instr :: instrs2)) @ [load_instr] in
          (instrs, Exp.Var id, type_of_expr)
      | other_binop ->
          let sil_binop = get_binop type_of_expr other_binop in
          let sil_expr = Exp.BinOp (sil_binop, sil_ex1, sil_ex2) in
          (instrs1 @ instrs2, sil_expr, type_of_expr) )
  | JBir.Field (ex, cn, fs) ->
      let instrs, sil_expr, _ = expression context pc ex in
      let field_name = get_field_name program false tenv cn fs in
      let sil_type = JTransType.get_class_type_no_pointer program tenv cn in
      let sil_expr = Exp.Lfield (sil_expr, field_name, sil_type) in
      let tmp_id = Ident.create_fresh Ident.knormal in
      let lderef_instr = Sil.Load (tmp_id, sil_expr, sil_type, loc) in
      (instrs @ [lderef_instr], Exp.Var tmp_id, type_of_expr)
  | JBir.StaticField (cn, fs) ->
      let class_exp =
        let classname = Mangled.from_string (JBasics.cn_name cn) in
        let var_name = Pvar.mk_global classname in
        Exp.Lvar var_name
      in
      let instrs, sil_expr = ([], class_exp) in
      let field_name = get_field_name program true tenv cn fs in
      let sil_type = JTransType.get_class_type_no_pointer program tenv cn in
      if JTransType.is_autogenerated_assert_field field_name then
        (* assume that reading from C.$assertionsDisabled always yields "false". this allows *)
        (* Infer to understand the assert keyword in the expected way *)
        (instrs, Exp.zero, type_of_expr)
      else
        let sil_expr = Exp.Lfield (sil_expr, field_name, sil_type) in
        let tmp_id = Ident.create_fresh Ident.knormal in
        let lderef_instr = Sil.Load (tmp_id, sil_expr, sil_type, loc) in
        (instrs @ [lderef_instr], Exp.Var tmp_id, type_of_expr)


let method_invocation (context : JContext.t) loc pc var_opt cn ms sil_obj_opt expr_list invoke_code
    method_kind =
  (* This function tries to recursively search for the classname of the class *)
  (* where the method is defined. It returns the classname given as argument*)
  (* when this classname cannot be found *)
  let resolve_method (context : JContext.t) cn ms =
    let rec loop fallback_cn cn =
      match JClasspath.lookup_node cn context.program with
      | None ->
          fallback_cn
      | Some node -> (
          if Javalib.defines_method node ms then cn
          else
            match node with
            | Javalib.JInterface _ ->
                fallback_cn
            | Javalib.JClass jclass -> (
              match jclass.Javalib.c_super_class with
              | None ->
                  fallback_cn
              | Some super_cn ->
                  loop fallback_cn super_cn ) )
    in
    loop cn cn
  in
  let cn' = resolve_method context cn ms in
  let tenv = JContext.get_tenv context in
  let program = context.program in
  let cf_virtual, cf_interface =
    match invoke_code with
    | I_Virtual ->
        (true, false)
    | I_Interface ->
        (true, true)
    | _ ->
        (false, false)
  in
  let call_flags = {CallFlags.default with cf_virtual; cf_interface} in
  let init =
    match sil_obj_opt with
    | None ->
        ([], [])
    | Some (sil_obj_expr, sil_obj_type) ->
        (* for non-constructors, add an instruction that dereferences the receiver *)
        let instrs =
          let is_non_constructor_call = match invoke_code with I_Special -> false | _ -> true in
          match sil_obj_expr with
          | Exp.Var _ when is_non_constructor_call && not Config.tracing ->
              let obj_typ_no_ptr =
                match sil_obj_type.Typ.desc with Typ.Tptr (typ, _) -> typ | _ -> sil_obj_type
              in
              [create_sil_deref sil_obj_expr obj_typ_no_ptr loc]
          | _ ->
              []
        in
        (instrs, [(sil_obj_expr, sil_obj_type)])
  in
  let instrs, call_args =
    List.fold
      ~f:(fun (instrs_accu, args_accu) expr ->
        let instrs, sil_expr, sil_expr_type = expression context pc expr in
        (instrs_accu @ instrs, args_accu @ [(sil_expr, sil_expr_type)]) )
      ~init expr_list
  in
  let callee_procname =
    let proc = Typ.Procname.from_string_c_fun (JBasics.ms_name ms) in
    if
      JBasics.cn_equal cn' (JBasics.make_cn JConfig.infer_builtins_cl)
      && BuiltinDecl.is_declared proc
    then proc
    else JTransType.get_method_procname program tenv cn' ms method_kind
  in
  let call_instrs =
    let callee_fun = Exp.Const (Const.Cfun callee_procname) in
    let return_type =
      match JBasics.ms_rtype ms with
      | None ->
          Typ.mk Tvoid
      | Some vt ->
          JTransType.value_type program tenv vt
    in
    let call_ret_instrs sil_var =
      let ret_id = Ident.create_fresh Ident.knormal in
      let call_instr = Sil.Call ((ret_id, return_type), callee_fun, call_args, loc, call_flags) in
      let set_instr = Sil.Store (Exp.Lvar sil_var, return_type, Exp.Var ret_id, loc) in
      instrs @ [call_instr; set_instr]
    in
    match var_opt with
    | None ->
        let call_instr =
          Sil.Call
            ( (Ident.create_fresh Ident.knormal, Typ.mk Tvoid)
            , callee_fun
            , call_args
            , loc
            , call_flags )
        in
        instrs @ [call_instr]
    | Some var ->
        let sil_var = JContext.set_pvar context var return_type in
        call_ret_instrs sil_var
  in
  let is_close = function
    | Typ.Procname.Java java_pname ->
        Typ.Procname.Java.is_close java_pname
    | _ ->
        false
  in
  (* return true for classes that are a subclass of Closeable, but don't actually represent a
     resource *)
  let is_non_resource_closeable typename _ =
    match Typ.Name.name typename with
    | "java.io.ByteArrayInputStream"
    | "java.io.ByteArrayOutputStream"
    | "java.io.StringReader"
    | "java.io.StringWriter" ->
        true
    | _ ->
        false
  in
  let instrs =
    match call_args with
    (* modeling a class bypasses the treatment of Closeable *)
    | _ when Config.models_mode || JClasspath.is_model callee_procname ->
        call_instrs
    | ((_, {Typ.desc= Typ.Tptr ({desc= Tstruct typename}, _)}) as exp) :: _
    (* add a file attribute when calling the constructor of a subtype of Closeable *)
      when Typ.Procname.is_constructor callee_procname
           && AndroidFramework.is_autocloseable tenv typename
           && not (PatternMatch.supertype_exists tenv is_non_resource_closeable typename) ->
        let set_file_attr =
          let set_builtin = Exp.Const (Const.Cfun BuiltinDecl.__set_file_attribute) in
          Sil.Call
            ( (Ident.create_fresh Ident.knormal, Typ.mk Tvoid)
            , set_builtin
            , [exp]
            , loc
            , CallFlags.default )
        in
        (* Exceptions thrown in the constructor should prevent adding the resource attribute *)
        call_instrs @ [set_file_attr]
    (* remove file attribute when calling the close method of a subtype of Closeable *)
    | [exp] when is_close callee_procname ->
        let set_mem_attr =
          let set_builtin = Exp.Const (Const.Cfun BuiltinDecl.__set_mem_attribute) in
          Sil.Call
            ( (Ident.create_fresh Ident.knormal, Typ.mk Tvoid)
            , set_builtin
            , [exp]
            , loc
            , CallFlags.default )
        in
        (* Exceptions thrown in the close method should not prevent the resource from being *)
        (* considered as closed *)
        [set_mem_attr] @ call_instrs
    | _ ->
        call_instrs
  in
  (callee_procname, instrs)


let get_array_length context pc expr_list content_type =
  let get_expr_instr expr other_instrs =
    let instrs, sil_len_expr, _ = expression context pc expr in
    match other_instrs with other_instrs, other_exprs ->
      (instrs @ other_instrs, sil_len_expr :: other_exprs)
  in
  let instrs, sil_len_exprs = List.fold_right ~f:get_expr_instr expr_list ~init:([], []) in
  let get_array_type_len sil_len_expr (content_type, _) =
    (Typ.mk_array content_type, Some sil_len_expr)
  in
  let array_type, array_len =
    List.fold_right ~f:get_array_type_len sil_len_exprs ~init:(content_type, None)
  in
  let array_size =
    Exp.Sizeof {typ= array_type; nbytes= None; dynamic_length= array_len; subtype= Subtype.exact}
  in
  (instrs, array_size)


let detect_loop entry_pc impl =
  let code = JBir.code impl in
  let pc_bound = Array.length code in
  let empty = Int.Set.empty in
  let rec loop visited pc =
    if Int.Set.mem visited pc || pc >= pc_bound then (false, visited)
    else
      let visited_updated = Int.Set.add visited pc in
      match code.(pc) with
      | JBir.Goto goto_pc when Int.equal goto_pc entry_pc ->
          (true, empty)
      | JBir.Goto goto_pc ->
          loop visited_updated goto_pc
      | JBir.Ifd (_, if_pc) when Int.equal if_pc entry_pc ->
          (true, empty)
      | JBir.Ifd (_, if_pc) ->
          let loop_detected, visited_after = loop visited_updated (pc + 1) in
          if loop_detected then (true, empty) else loop visited_after if_pc
      | _ ->
          if Int.equal (pc + 1) entry_pc then (true, empty) else loop visited_updated (pc + 1)
  in
  fst (loop empty entry_pc)


type translation =
  | Skip
  | Instr of Procdesc.Node.t
  | Prune of Procdesc.Node.t * Procdesc.Node.t
  | Loop of Procdesc.Node.t * Procdesc.Node.t * Procdesc.Node.t

let is_this expr =
  match expr with
  | JBir.Var (_, var) -> (
    match JBir.var_name_debug var with
    | None ->
        false
    | Some name_opt ->
        String.equal (Mangled.to_string JConfig.this) name_opt )
  | _ ->
      false


let assume_not_null loc sil_expr =
  let builtin_infer_assume = Exp.Const (Const.Cfun BuiltinDecl.__infer_assume) in
  let not_null_expr = Exp.BinOp (Binop.Ne, sil_expr, Exp.null) in
  let assume_call_flag = {CallFlags.default with CallFlags.cf_noreturn= true} in
  let call_args = [(not_null_expr, Typ.mk (Tint Typ.IBool))] in
  Sil.Call
    ( (Ident.create_fresh Ident.knormal, Typ.mk Tvoid)
    , builtin_infer_assume
    , call_args
    , loc
    , assume_call_flag )


let instruction (context : JContext.t) pc instr : translation =
  let tenv = JContext.get_tenv context in
  let program = context.program in
  let proc_name = Procdesc.get_proc_name context.procdesc in
  let ret_var = Pvar.get_ret_pvar proc_name in
  let ret_type = Procdesc.get_ret_type context.procdesc in
  let loc = get_location context.source_file context.impl pc in
  let match_never_null = Inferconfig.never_return_null_matcher in
  let create_node node_kind sil_instrs =
    Procdesc.create_node context.procdesc loc node_kind sil_instrs
  in
  let return_not_null () = match_never_null loc.Location.file proc_name in
  let trans_monitor_enter_exit context expr pc loc builtin node_desc =
    let instrs, sil_expr, sil_type = expression context pc expr in
    let builtin_const = Exp.Const (Const.Cfun builtin) in
    let instr =
      Sil.Call
        ( (Ident.create_fresh Ident.knormal, Typ.mk Tvoid)
        , builtin_const
        , [(sil_expr, sil_type)]
        , loc
        , CallFlags.default )
    in
    let typ_no_ptr = match sil_type.Typ.desc with Typ.Tptr (typ, _) -> typ | _ -> sil_type in
    let deref_instr = create_sil_deref sil_expr typ_no_ptr loc in
    let node_kind = Procdesc.Node.Stmt_node node_desc in
    Instr (create_node node_kind (instrs @ [deref_instr; instr]))
  in
  let create_node_kind procname =
    let procname_string = Typ.Procname.to_string procname in
    Procdesc.Node.Stmt_node (Call procname_string)
  in
  try
    match instr with
    | JBir.AffectVar (var, expr) ->
        let stml, sil_expr, sil_type = expression context pc expr in
        let pvar = JContext.set_pvar context var sil_type in
        let sil_instr = Sil.Store (Exp.Lvar pvar, sil_type, sil_expr, loc) in
        let node_kind = Procdesc.Node.Stmt_node MethodBody in
        let node = create_node node_kind (stml @ [sil_instr]) in
        Instr node
    | JBir.Return expr_option ->
        let node_kind = Procdesc.Node.Stmt_node MethodBody in
        let node =
          match expr_option with
          | None ->
              create_node node_kind []
          | Some expr ->
              let stml, sil_expr, _ = expression context pc expr in
              let sil_instrs =
                let return_instr = Sil.Store (Exp.Lvar ret_var, ret_type, sil_expr, loc) in
                if return_not_null () then [assume_not_null loc sil_expr; return_instr]
                else [return_instr]
              in
              create_node node_kind (stml @ sil_instrs)
        in
        JContext.add_goto_jump context pc JContext.Exit ;
        Instr node
    | JBir.AffectArray (array_ex, index_ex, value_ex) ->
        let instrs_array, sil_expr_array, _ = expression context pc array_ex
        and instrs_index, sil_expr_index, _ = expression context pc index_ex
        and instrs_value, sil_expr_value, value_typ = expression context pc value_ex in
        let sil_instr =
          Sil.Store (Exp.Lindex (sil_expr_array, sil_expr_index), value_typ, sil_expr_value, loc)
        in
        let final_instrs = instrs_array @ instrs_index @ instrs_value @ [sil_instr] in
        let node_kind = Procdesc.Node.Stmt_node MethodBody in
        let node = create_node node_kind final_instrs in
        Instr node
    | JBir.AffectField (e_lhs, cn, fs, e_rhs) ->
        let stml1, sil_expr_lhs, _ = expression context pc e_lhs in
        let stml2, sil_expr_rhs, _ = expression context pc e_rhs in
        let field_name = get_field_name program false tenv cn fs in
        let type_of_the_surrounding_class = JTransType.get_class_type_no_pointer program tenv cn in
        let type_of_the_root_of_e_lhs = type_of_the_surrounding_class in
        let expr_off = Exp.Lfield (sil_expr_lhs, field_name, type_of_the_surrounding_class) in
        let sil_instr = Sil.Store (expr_off, type_of_the_root_of_e_lhs, sil_expr_rhs, loc) in
        let node_kind = Procdesc.Node.Stmt_node MethodBody in
        let node = create_node node_kind (stml1 @ stml2 @ [sil_instr]) in
        Instr node
    | JBir.AffectStaticField (cn, fs, e_rhs) ->
        let class_exp =
          let classname = Mangled.from_string (JBasics.cn_name cn) in
          let var_name = Pvar.mk_global classname in
          Exp.Lvar var_name
        in
        let stml1, sil_expr_lhs = ([], class_exp) in
        let stml2, sil_expr_rhs, _ = expression context pc e_rhs in
        let field_name = get_field_name program true tenv cn fs in
        let type_of_the_surrounding_class = JTransType.get_class_type_no_pointer program tenv cn in
        let type_of_the_root_of_e_lhs = type_of_the_surrounding_class in
        let expr_off = Exp.Lfield (sil_expr_lhs, field_name, type_of_the_surrounding_class) in
        let sil_instr = Sil.Store (expr_off, type_of_the_root_of_e_lhs, sil_expr_rhs, loc) in
        let node_kind = Procdesc.Node.Stmt_node MethodBody in
        let node = create_node node_kind (stml1 @ stml2 @ [sil_instr]) in
        Instr node
    | JBir.Goto goto_pc ->
        JContext.reset_pvar_type context ;
        JContext.add_goto_jump context pc (JContext.Jump goto_pc) ;
        Skip
    | JBir.Ifd ((op, e1, e2), if_pc) ->
        (* Note: JBir provides the condition for the false branch, under which to jump *)
        JContext.reset_pvar_type context ;
        let instrs1, sil_ex1, _ = expression context pc e1
        and instrs2, sil_ex2, _ = expression context pc e2 in
        let sil_op = get_test_operator op in
        let sil_test_false = Exp.BinOp (sil_op, sil_ex1, sil_ex2) in
        let sil_test_true = Exp.UnOp (Unop.LNot, sil_test_false, None) in
        let sil_instrs_true = Sil.Prune (sil_test_true, loc, true, Sil.Ik_if) in
        let sil_instrs_false = Sil.Prune (sil_test_false, loc, false, Sil.Ik_if) in
        let node_kind_true =
          Procdesc.Node.Prune_node (true, Sil.Ik_if, PruneNodeKind_MethodBody)
        in
        let node_kind_false =
          Procdesc.Node.Prune_node (false, Sil.Ik_if, PruneNodeKind_MethodBody)
        in
        let prune_node_true = create_node node_kind_true (instrs1 @ instrs2 @ [sil_instrs_true])
        and prune_node_false =
          create_node node_kind_false (instrs1 @ instrs2 @ [sil_instrs_false])
        in
        JContext.add_if_jump context prune_node_false if_pc ;
        if detect_loop pc context.impl then
          let join_node_kind = Procdesc.Node.Join_node in
          let join_node = create_node join_node_kind [] in
          Loop (join_node, prune_node_true, prune_node_false)
        else Prune (prune_node_true, prune_node_false)
    | JBir.Throw expr ->
        let instrs, sil_expr, _ = expression context pc expr in
        let sil_exn = Exp.Exn sil_expr in
        let sil_instr = Sil.Store (Exp.Lvar ret_var, ret_type, sil_exn, loc) in
        let node = create_node Procdesc.Node.throw_kind (instrs @ [sil_instr]) in
        JContext.add_goto_jump context pc JContext.Exit ;
        Instr node
    | JBir.New (var, cn, constr_type_list, constr_arg_list) ->
        let builtin_new = Exp.Const (Const.Cfun BuiltinDecl.__new) in
        let class_type = JTransType.get_class_type program tenv cn in
        let class_type_np = JTransType.get_class_type_no_pointer program tenv cn in
        let sizeof_exp =
          Exp.Sizeof
            {typ= class_type_np; nbytes= None; dynamic_length= None; subtype= Subtype.exact}
        in
        let args = [(sizeof_exp, class_type)] in
        let ret_id = Ident.create_fresh Ident.knormal in
        let new_instr =
          Sil.Call ((ret_id, class_type), builtin_new, args, loc, CallFlags.default)
        in
        let constr_ms = JBasics.make_ms JConfig.constructor_name constr_type_list None in
        let constr_procname, call_instrs =
          let ret_opt = Some (Exp.Var ret_id, class_type) in
          method_invocation context loc pc None cn constr_ms ret_opt constr_arg_list I_Special
            Typ.Procname.Java.Non_Static
        in
        let pvar = JContext.set_pvar context var class_type in
        let set_instr = Sil.Store (Exp.Lvar pvar, class_type, Exp.Var ret_id, loc) in
        let instrs = (new_instr :: call_instrs) @ [set_instr] in
        let node_kind = create_node_kind constr_procname in
        let node = create_node node_kind instrs in
        Instr node
    | JBir.NewArray (var, vt, expr_list) ->
        let builtin_new_array = Exp.Const (Const.Cfun BuiltinDecl.__new_array) in
        let content_type = JTransType.value_type program tenv vt in
        let array_type = JTransType.create_array_type content_type (List.length expr_list) in
        let array_name = JContext.set_pvar context var array_type in
        let instrs, array_size = get_array_length context pc expr_list content_type in
        let call_args = [(array_size, array_type)] in
        let ret_id = Ident.create_fresh Ident.knormal in
        let call_instr =
          Sil.Call ((ret_id, array_type), builtin_new_array, call_args, loc, CallFlags.default)
        in
        let set_instr = Sil.Store (Exp.Lvar array_name, array_type, Exp.Var ret_id, loc) in
        let node_kind = Procdesc.Node.Stmt_node MethodBody in
        let node = create_node node_kind (instrs @ [call_instr; set_instr]) in
        Instr node
    | JBir.InvokeStatic (var_opt, cn, ms, args) ->
        let sil_obj_opt, args, instrs =
          match args with
          | [arg] when is_clone ms ->
              (* hack to null check the receiver of clone when clone is an array. in the array.clone()
                 case, clone is a virtual call that we translate as a static call *)
              let instrs, sil_arg_expr, arg_typ = expression context pc arg in
              (Some (sil_arg_expr, arg_typ), [], instrs)
          | _ ->
              (None, args, [])
        in
        let callee_procname, call_instrs =
          method_invocation context loc pc var_opt cn ms sil_obj_opt args I_Static
            Typ.Procname.Java.Static
        in
        let node_kind = create_node_kind callee_procname in
        let call_node = create_node node_kind (instrs @ call_instrs) in
        Instr call_node
    | JBir.InvokeVirtual (var_opt, obj, call_kind, ms, args) -> (
        let instrs, sil_obj_expr, sil_obj_type = expression context pc obj in
        let create_call_node cn invoke_kind =
          let callee_procname, call_instrs =
            let ret_opt = Some (sil_obj_expr, sil_obj_type) in
            method_invocation context loc pc var_opt cn ms ret_opt args invoke_kind
              Typ.Procname.Java.Non_Static
          in
          let node_kind = create_node_kind callee_procname in
          let call_node = create_node node_kind (instrs @ call_instrs) in
          call_node
        in
        let trans_virtual_call original_cn invoke_kind =
          let cn' =
            match JTransType.extract_cn_no_obj sil_obj_type with
            | Some cn ->
                cn
            | None ->
                original_cn
          in
          let call_node = create_call_node cn' invoke_kind in
          Instr call_node
        in
        match call_kind with
        | JBir.VirtualCall obj_type ->
            let cn =
              match obj_type with
              | JBasics.TClass cn ->
                  cn
              | JBasics.TArray _ ->
                  JBasics.java_lang_object
            in
            trans_virtual_call cn I_Virtual
        | JBir.InterfaceCall cn ->
            trans_virtual_call cn I_Interface )
    | JBir.InvokeNonVirtual (var_opt, obj, cn, ms, args) ->
        let instrs, sil_obj_expr, sil_obj_type = expression context pc obj in
        let callee_procname, call_instrs =
          method_invocation context loc pc var_opt cn ms
            (Some (sil_obj_expr, sil_obj_type))
            args I_Special Typ.Procname.Java.Non_Static
        in
        let node_kind = create_node_kind callee_procname in
        let call_node = create_node node_kind (instrs @ call_instrs) in
        Instr call_node
    | JBir.Check (JBir.CheckNullPointer expr) when Config.tracing && is_this expr ->
        (* TODO #6509339: refactor the boilerplate code in the translation of JVM checks *)
        let instrs, sil_expr, _ = expression context pc expr in
        let this_not_null_node =
          create_node (Procdesc.Node.Stmt_node ThisNotNull)
            (instrs @ [assume_not_null loc sil_expr])
        in
        Instr this_not_null_node
    | JBir.Check (JBir.CheckNullPointer expr) when Config.tracing ->
        let instrs, sil_expr, _ = expression context pc expr in
        let not_null_node =
          let sil_not_null = Exp.BinOp (Binop.Ne, sil_expr, Exp.null) in
          let sil_prune_not_null = Sil.Prune (sil_not_null, loc, true, Sil.Ik_if)
          and not_null_kind = Procdesc.Node.Prune_node (true, Sil.Ik_if, PruneNodeKind_NotNull) in
          create_node not_null_kind (instrs @ [sil_prune_not_null])
        in
        let throw_npe_node =
          let sil_is_null = Exp.BinOp (Binop.Eq, sil_expr, Exp.null) in
          let sil_prune_null = Sil.Prune (sil_is_null, loc, true, Sil.Ik_if)
          and npe_kind = Procdesc.Node.Stmt_node ThrowNPE
          and npe_cn = JBasics.make_cn JConfig.npe_cl in
          let class_type = JTransType.get_class_type program tenv npe_cn
          and class_type_np = JTransType.get_class_type_no_pointer program tenv npe_cn in
          let sizeof_exp =
            Exp.Sizeof
              {typ= class_type_np; nbytes= None; dynamic_length= None; subtype= Subtype.exact}
          in
          let args = [(sizeof_exp, class_type)] in
          let ret_id = Ident.create_fresh Ident.knormal in
          let new_instr =
            Sil.Call ((ret_id, class_type), builtin_new, args, loc, CallFlags.default)
          in
          let constr_ms = JBasics.make_ms JConfig.constructor_name [] None in
          let _, call_instrs =
            let ret_opt = Some (Exp.Var ret_id, class_type) in
            method_invocation context loc pc None npe_cn constr_ms ret_opt [] I_Special
              Typ.Procname.Java.Static
          in
          let sil_exn = Exp.Exn (Exp.Var ret_id) in
          let set_instr = Sil.Store (Exp.Lvar ret_var, ret_type, sil_exn, loc) in
          let npe_instrs = instrs @ [sil_prune_null] @ (new_instr :: call_instrs) @ [set_instr] in
          create_node npe_kind npe_instrs
        in
        Prune (not_null_node, throw_npe_node)
    | JBir.Check (JBir.CheckArrayBound (array_expr, index_expr)) when Config.tracing ->
        let instrs, _, sil_length_expr, sil_index_expr =
          let array_instrs, sil_array_expr, _ = expression context pc array_expr
          and length_instrs, sil_length_expr, _ =
            expression context pc (JBir.Unop (JBir.ArrayLength, array_expr))
          and index_instrs, sil_index_expr, _ = expression context pc index_expr in
          let instrs = array_instrs @ index_instrs @ length_instrs in
          (instrs, sil_array_expr, sil_length_expr, sil_index_expr)
        in
        let in_bound_node =
          let in_bound_node_kind =
            Procdesc.Node.Prune_node (true, Sil.Ik_if, PruneNodeKind_InBound)
          in
          let sil_assume_in_bound =
            let sil_in_bound =
              let sil_positive_index =
                Exp.BinOp (Binop.Ge, sil_index_expr, Exp.Const (Const.Cint IntLit.zero))
              and sil_less_than_length = Exp.BinOp (Binop.Lt, sil_index_expr, sil_length_expr) in
              Exp.BinOp (Binop.LAnd, sil_positive_index, sil_less_than_length)
            in
            Sil.Prune (sil_in_bound, loc, true, Sil.Ik_if)
          in
          create_node in_bound_node_kind (instrs @ [sil_assume_in_bound])
        and throw_out_of_bound_node =
          let out_of_bound_node_kind = Procdesc.Node.Stmt_node OutOfBound in
          let sil_assume_out_of_bound =
            let sil_out_of_bound =
              let sil_negative_index =
                Exp.BinOp (Binop.Lt, sil_index_expr, Exp.Const (Const.Cint IntLit.zero))
              and sil_greater_than_length =
                Exp.BinOp (Binop.Gt, sil_index_expr, sil_length_expr)
              in
              Exp.BinOp (Binop.LOr, sil_negative_index, sil_greater_than_length)
            in
            Sil.Prune (sil_out_of_bound, loc, true, Sil.Ik_if)
          in
          let out_of_bound_cn = JBasics.make_cn JConfig.out_of_bound_cl in
          let class_type = JTransType.get_class_type program tenv out_of_bound_cn
          and class_type_np = JTransType.get_class_type_no_pointer program tenv out_of_bound_cn in
          let sizeof_exp =
            Exp.Sizeof
              {typ= class_type_np; nbytes= None; dynamic_length= None; subtype= Subtype.exact}
          in
          let args = [(sizeof_exp, class_type)] in
          let ret_id = Ident.create_fresh Ident.knormal in
          let new_instr =
            Sil.Call ((ret_id, ret_type), builtin_new, args, loc, CallFlags.default)
          in
          let constr_ms = JBasics.make_ms JConfig.constructor_name [] None in
          let _, call_instrs =
            method_invocation context loc pc None out_of_bound_cn constr_ms
              (Some (Exp.Var ret_id, class_type))
              [] I_Special Typ.Procname.Java.Static
          in
          let sil_exn = Exp.Exn (Exp.Var ret_id) in
          let set_instr = Sil.Store (Exp.Lvar ret_var, ret_type, sil_exn, loc) in
          let out_of_bound_instrs =
            instrs @ [sil_assume_out_of_bound] @ (new_instr :: call_instrs) @ [set_instr]
          in
          create_node out_of_bound_node_kind out_of_bound_instrs
        in
        Prune (in_bound_node, throw_out_of_bound_node)
    | JBir.Check (JBir.CheckCast (expr, object_type)) when Config.tracing ->
        let sil_type = JTransType.expr_type context expr
        and instrs, sil_expr, _ = expression context pc expr
        and ret_id = Ident.create_fresh Ident.knormal
        and sizeof_expr =
          JTransType.sizeof_of_object_type program tenv object_type Subtype.subtypes_instof
        in
        let check_cast = Exp.Const (Const.Cfun BuiltinDecl.__instanceof) in
        let args = [(sil_expr, sil_type); (sizeof_expr, Typ.mk Tvoid)] in
        let call = Sil.Call ((ret_id, ret_type), check_cast, args, loc, CallFlags.default) in
        let res_ex = Exp.Var ret_id in
        let is_instance_node =
          let check_is_false = Exp.BinOp (Binop.Ne, res_ex, Exp.zero) in
          let asssume_instance_of = Sil.Prune (check_is_false, loc, true, Sil.Ik_if)
          and instance_of_kind =
            Procdesc.Node.Prune_node (true, Sil.Ik_if, PruneNodeKind_IsInstance)
          in
          create_node instance_of_kind (instrs @ [call; asssume_instance_of])
        and throw_cast_exception_node =
          let check_is_true = Exp.BinOp (Binop.Ne, res_ex, Exp.one) in
          let asssume_not_instance_of = Sil.Prune (check_is_true, loc, true, Sil.Ik_if)
          and throw_cast_exception_kind = Procdesc.Node.Stmt_node ClassCastException
          and cce_cn = JBasics.make_cn JConfig.cce_cl in
          let class_type = JTransType.get_class_type program tenv cce_cn
          and class_type_np = JTransType.get_class_type_no_pointer program tenv cce_cn in
          let sizeof_exp =
            Exp.Sizeof
              {typ= class_type_np; nbytes= None; dynamic_length= None; subtype= Subtype.exact}
          in
          let args = [(sizeof_exp, class_type)] in
          let ret_id = Ident.create_fresh Ident.knormal in
          let new_instr =
            Sil.Call ((ret_id, ret_type), builtin_new, args, loc, CallFlags.default)
          in
          let constr_ms = JBasics.make_ms JConfig.constructor_name [] None in
          let _, call_instrs =
            method_invocation context loc pc None cce_cn constr_ms
              (Some (Exp.Var ret_id, class_type))
              [] I_Special Typ.Procname.Java.Static
          in
          let sil_exn = Exp.Exn (Exp.Var ret_id) in
          let set_instr = Sil.Store (Exp.Lvar ret_var, ret_type, sil_exn, loc) in
          let cce_instrs =
            instrs @ [call; asssume_not_instance_of] @ (new_instr :: call_instrs) @ [set_instr]
          in
          create_node throw_cast_exception_kind cce_instrs
        in
        Prune (is_instance_node, throw_cast_exception_node)
    | JBir.MonitorEnter expr ->
        trans_monitor_enter_exit context expr pc loc BuiltinDecl.__set_locked_attribute
          MonitorEnter
    | JBir.MonitorExit expr ->
        trans_monitor_enter_exit context expr pc loc BuiltinDecl.__delete_locked_attribute
          MonitorExit
    | _ ->
        Skip
  with Frontend_error s ->
    L.internal_error "Skipping because of: %s@." s ;
    Skip
