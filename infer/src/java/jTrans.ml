(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

open Javalib_pack
open Sawja_pack

module L = Logging

type invoke_kind =
  | I_Virtual
  | I_Interface
  | I_Special
  | I_Static

exception Frontend_error of string

let constr_loc_map : Location.t JBasics.ClassMap.t ref = ref JBasics.ClassMap.empty

let init_loc_map : Location.t JBasics.ClassMap.t ref = ref JBasics.ClassMap.empty

(** Fix the line associated to a method definition.
    Since Sawja often reports a method off by a few lines, we search
    backwards for a line where the method name is. *)
let fix_method_definition_line linereader proc_name_java loc =
  let proc_name = Procname.Java proc_name_java in
  let method_name =
    if Procname.is_constructor proc_name then
      let inner_class_name cname = snd (string_split_character cname '$') in
      inner_class_name (Procname.java_get_simple_class_name proc_name_java)
    else Procname.java_get_method proc_name_java in
  let regex = Str.regexp (Str.quote method_name) in
  let method_is_defined_here linenum =
    match Printer.LineReader.from_file_linenum_original linereader loc.Location.file linenum with
    | None -> raise Not_found
    | Some line ->
        (try ignore (Str.search_forward regex line 0); true
         with Not_found -> false) in
  let line = ref loc.Location.line in
  try
    while not (method_is_defined_here !line) do
      line := !line -1;
      if !line < 0 then raise Not_found
    done;
    { loc with Location.line = !line }
  with Not_found -> loc

let get_location impl pc meth_kind cn =
  if meth_kind = JContext.Init then
    try
      JBasics.ClassMap.find cn !init_loc_map
    with Not_found -> Location.dummy
  else
    let line_number =
      let ln =
        try JBir.get_source_line_number pc impl
        with Invalid_argument _ -> None in
      match ln with
      | None -> 0
      | Some n -> n in
    { Location.line = line_number;
      col = -1;
      file = !DB.current_source;
      nLOC = !Config.nLOC }

let get_undefined_method_call ovt =
  let get_undefined_method ovt =
    match ovt with
    | None -> JConfig.void^"_undefined"
    | Some vt ->
        match vt with
        | JBasics.TBasic bt -> (JTransType.string_of_basic_type bt)^"_undefined"
        | JBasics.TObject ot ->
            begin
              match ot with
              | JBasics.TArray _ -> assert false
              | JBasics.TClass cn ->
                  if JBasics.cn_name cn = JConfig.string_cl then
                    "string_undefined"
                  else
                  if JBasics.cn_name cn = JConfig.object_cl then
                    "object_undefined"
                  else assert false
            end in
  let undef_cn = JBasics.make_cn JConfig.infer_undefined_cl in
  let undef_name = get_undefined_method ovt in
  let undef_ms = JBasics.make_ms undef_name [] ovt in
  (undef_cn, undef_ms)


let retrieve_fieldname fieldname =
  try
    let subs = Str.split (Str.regexp (Str.quote ".")) (Ident.fieldname_to_string fieldname) in
    if IList.length subs = 0 then
      assert false
    else
      IList.hd (IList.rev subs)
  with _ -> assert false


let get_field_name program static tenv cn fs =
  match JTransType.get_class_type_no_pointer program tenv cn with
  | Sil.Tstruct { Sil.instance_fields; static_fields; csu = Csu.Class _ } ->
      let fieldname, _, _ =
        try
          IList.find
            (fun (fieldname, _, _) -> retrieve_fieldname fieldname = JBasics.fs_name fs)
            (if static then static_fields else instance_fields)
        with Not_found ->
          (* TODO: understand why fields cannot be found here *)
          JUtils.log "cannot find %s.%s@." (JBasics.cn_name cn) (JBasics.fs_name fs);
          raise (Frontend_error "Cannot find fieldname") in
      fieldname
  | _ -> assert false


let formals_from_signature program tenv cn ms kind =
  let counter = ref 0 in
  let method_name = JBasics.ms_name ms in
  let get_arg_name () =
    let arg = method_name^"_arg_"^(string_of_int !counter) in
    incr counter;
    Mangled.from_string arg in
  let collect l vt =
    let arg_name = get_arg_name () in
    let arg_type = JTransType.value_type program tenv vt in
    (arg_name, arg_type):: l in
  let init_arg_list = match kind with
    | Procname.Static -> []
    | Procname.Non_Static -> [(JConfig.this, JTransType.get_class_type program tenv cn)] in
  IList.rev (IList.fold_left collect init_arg_list (JBasics.ms_args ms))

let formals program tenv cn impl =
  let collect l (vt, var) =
    let name = Mangled.from_string (JBir.var_name_g var) in
    let typ = JTransType.param_type program tenv cn var vt in
    (name, typ):: l in
  IList.rev (IList.fold_left collect [] (JBir.params impl))

(** Creates the local and formal variables from a procedure based on the
    impl argument. If the meth_kind is Init, we add a parameter field to
    the initialiser method. *)
let locals_formals program tenv cn impl meth_kind =
  let form_list =
    if meth_kind = JContext.Init then
      let string_type = (JTransType.get_class_type program tenv (JBasics.make_cn JConfig.string_cl)) in
      [(JConfig.field_st, string_type) ]
    else formals program tenv cn impl in
  let is_formal p =
    IList.exists (fun (p', _) -> Mangled.equal p p') form_list in
  let collect l var =
    let vname = Mangled.from_string (JBir.var_name_g var) in
    let names = (fst (IList.split l)) in
    if not (is_formal vname) && (not (IList.mem Mangled.equal vname names)) then
      (vname, Sil.Tvoid):: l
    else
      l in
  let vars = JBir.vars impl in
  let loc_list = IList.rev (Array.fold_left collect [] vars) in
  (loc_list, form_list)

let get_constant (c : JBir.const) =
  match c with
  | `Int i -> Sil.Cint (Sil.Int.of_int32 i)
  | `ANull -> Sil.Cint Sil.Int.null
  | `Class ot -> Sil.Cclass (Ident.string_to_name (JTransType.object_type_to_string ot))
  | `Double f -> Sil.Cfloat f
  | `Float f -> Sil.Cfloat f
  | `Long i64 -> Sil.Cint (Sil.Int.of_int64 i64)
  | `String jstr -> Sil.Cstr (JBasics.jstr_pp jstr)

let get_binop binop =
  match binop with
  | JBir.Add _ -> Sil.PlusA
  | JBir.Sub _ -> Sil.MinusA
  | JBir.Mult _ -> Sil.Mult
  | JBir.Div _ -> Sil.Div
  | JBir.Rem _ -> Sil.Mod
  | JBir.IAnd -> Sil.BAnd
  | JBir.IShl -> Sil.Shiftlt
  | JBir.IShr -> Sil.Shiftrt
  | JBir.IOr -> Sil.BOr
  | JBir.IXor -> Sil.BXor
  | JBir.IUshr ->
      raise (Frontend_error "Unsigned right shift operator")
  | JBir.LShl -> Sil.Shiftlt
  | JBir.LShr -> Sil.Shiftrt
  | JBir.LAnd -> Sil.BAnd
  | JBir.LOr -> Sil.BOr
  | JBir.LXor -> Sil.BXor
  | JBir.LUshr ->
      raise (Frontend_error "Unsigned right shift operator")
  | JBir.CMP _ ->
      raise (Frontend_error "Unsigned right shift operator")
  | JBir.ArrayLoad _ ->
      raise (Frontend_error "Array load operator")

let get_test_operator op =
  match op with
  | `Eq -> Sil.Eq
  | `Ge -> Sil.Ge
  | `Gt -> Sil.Gt
  | `Le -> Sil.Le
  | `Lt -> Sil.Lt
  | `Ne -> Sil.Ne

type defined_status =
  | Defined of Cfg.Procdesc.t
  | Called of Cfg.Procdesc.t

type translation_status =
  | Created of defined_status
  | Unknown

let lookup_procdesc cfg procname =
  match Cfg.Procdesc.find_from_name cfg procname with
  | Some procdesc ->
      if Cfg.Procdesc.is_defined procdesc then
        Created (Defined procdesc)
      else
        Created (Called procdesc)
  | None -> Unknown

let is_java_native cm =
  (cm.Javalib.cm_implementation = Javalib.Native)

let is_clone ms =
  JBasics.ms_name ms = JConfig.clone_name

let get_implementation cm =
  match cm.Javalib.cm_implementation with
  | Javalib.Native ->
      let cms = cm.Javalib.cm_class_method_signature in
      let cn, ms = JBasics.cms_split cms in
      JUtils.log "native method %s found in %s@." (JBasics.ms_name ms) (JBasics.cn_name cn);
      assert false
  | Javalib.Java t ->
      JBir.transform ~bcv: false ~ch_link: false ~formula: false ~formula_cmd:[] cm (Lazy.force t)

let update_constr_loc cn ms loc_start =
  if (JBasics.ms_name ms) = JConfig.constructor_name then
    try ignore(JBasics.ClassMap.find cn !constr_loc_map)
    with Not_found -> constr_loc_map := (JBasics.ClassMap.add cn loc_start !constr_loc_map)

let update_init_loc cn ms loc_start =
  if JBasics.ms_equal ms JBasics.clinit_signature then
    try ignore(JBasics.ClassMap.find cn !init_loc_map)
    with Not_found -> init_loc_map := (JBasics.ClassMap.add cn loc_start !init_loc_map)

let no_static_final = ref false

(** Creates a procedure description. *)
let create_local_procdesc program linereader cfg tenv node m =
  let cn, ms = JBasics.cms_split (Javalib.get_class_method_signature m) in
  let meth_kind =
    if JBasics.ms_equal ms JBasics.clinit_signature then JContext.Init
    else JContext.Normal in
  if not (
      !no_static_final = false &&
      meth_kind = JContext.Init &&
      not (JTransStaticField.has_static_final_fields node))
  then
    let proc_name_java = JTransType.get_method_procname cn ms (JTransType.get_method_kind m) in
    let proc_name = Procname.Java proc_name_java in
    let create_new_procdesc () =
      let trans_access = function
        | `Default -> Sil.Default
        | `Public -> Sil.Public
        | `Private -> Sil.Private
        | `Protected -> Sil.Protected in
      try
        match m with
        | Javalib.AbstractMethod am -> (* create a procdesc with empty body *)
            let formals =
              formals_from_signature program tenv cn ms (JTransType.get_method_kind m) in
            let method_annotation =
              JAnnotation.translate_method proc_name_java am.Javalib.am_annotations in
            let procdesc =
              let proc_attributes =
                { (ProcAttributes.default proc_name Config.Java) with
                  ProcAttributes.access = trans_access am.Javalib.am_access;
                  exceptions = IList.map JBasics.cn_name am.Javalib.am_exceptions;
                  formals;
                  is_abstract = true;
                  is_bridge_method = am.Javalib.am_bridge;
                  is_defined = true;
                  is_synthetic_method = am.Javalib.am_synthetic;
                  method_annotation;
                  ret_type = JTransType.return_type program tenv ms meth_kind;
                } in
              Cfg.Procdesc.create cfg proc_attributes in
            let start_kind = Cfg.Node.Start_node procdesc in
            let start_node = Cfg.Node.create cfg Location.dummy start_kind [] procdesc [] in
            let exit_kind = (Cfg.Node.Exit_node procdesc) in
            let exit_node = Cfg.Node.create cfg Location.dummy exit_kind [] procdesc [] in
            Cfg.Node.set_succs_exn cfg start_node [exit_node] [exit_node];
            Cfg.Procdesc.set_start_node procdesc start_node;
            Cfg.Procdesc.set_exit_node procdesc exit_node
        | Javalib.ConcreteMethod cm when is_java_native cm ->
            let formals = formals_from_signature program tenv cn ms (JTransType.get_method_kind m) in
            let method_annotation =
              JAnnotation.translate_method proc_name_java cm.Javalib.cm_annotations in
            let proc_attributes =
              { (ProcAttributes.default proc_name Config.Java) with
                ProcAttributes.access = trans_access cm.Javalib.cm_access;
                exceptions = IList.map JBasics.cn_name cm.Javalib.cm_exceptions;
                formals;
                is_bridge_method = cm.Javalib.cm_bridge;
                is_synthetic_method = cm.Javalib.cm_synthetic;
                method_annotation;
                ret_type = JTransType.return_type program tenv ms meth_kind;
              } in
            ignore (Cfg.Procdesc.create cfg proc_attributes)
        | Javalib.ConcreteMethod cm ->
            let impl = get_implementation cm in
            let locals, formals = locals_formals program tenv cn impl meth_kind in
            let loc_start =
              let loc = (get_location impl 0 JContext.Normal cn) in
              fix_method_definition_line linereader proc_name_java loc in
            let loc_exit = (get_location impl (Array.length (JBir.code impl) - 1) JContext.Normal cn) in
            let method_annotation =
              JAnnotation.translate_method proc_name_java cm.Javalib.cm_annotations in
            update_constr_loc cn ms loc_start;
            update_init_loc cn ms loc_exit;
            let procdesc =
              let proc_attributes =
                { (ProcAttributes.default proc_name Config.Java) with
                  ProcAttributes.access = trans_access cm.Javalib.cm_access;
                  exceptions = IList.map JBasics.cn_name cm.Javalib.cm_exceptions;
                  formals;
                  is_bridge_method = cm.Javalib.cm_bridge;
                  is_defined = true;
                  is_synthetic_method = cm.Javalib.cm_synthetic;
                  loc = loc_start;
                  locals;
                  method_annotation;
                  ret_type = JTransType.return_type program tenv ms meth_kind;
                } in
              Cfg.Procdesc.create cfg proc_attributes in
            let start_kind = Cfg.Node.Start_node procdesc in
            let start_node = Cfg.Node.create cfg loc_start start_kind [] procdesc [] in
            let exit_kind = (Cfg.Node.Exit_node procdesc) in
            let exit_node = Cfg.Node.create cfg loc_exit exit_kind [] procdesc [] in
            let exn_kind = Cfg.Node.exn_sink_kind in
            let exn_node = Cfg.Node.create cfg loc_exit exn_kind [] procdesc [] in
            JContext.add_exn_node proc_name exn_node;
            Cfg.Procdesc.set_start_node procdesc start_node;
            Cfg.Procdesc.set_exit_node procdesc exit_node;
            Cfg.Node.add_locals_ret_declaration start_node locals;
      with JBir.Subroutine ->
        L.err "create_local_procdesc raised JBir.Subroutine on %a@." Procname.pp proc_name in
    match lookup_procdesc cfg proc_name with
    | Unknown ->
        create_new_procdesc ()
    | Created defined_status ->
        begin
          match defined_status with
          | Defined _ -> assert false
          | Called procdesc ->
              Cfg.Procdesc.remove cfg (Cfg.Procdesc.get_proc_name procdesc) false;
              create_new_procdesc ()
        end

let create_external_procdesc program cfg tenv cn ms kind =
  let return_type =
    match JBasics.ms_rtype ms with
    | None -> Sil.Tvoid
    | Some vt -> JTransType.value_type program tenv vt in
  let formals = formals_from_signature program tenv cn ms kind in
  let proc_name_java = JTransType.get_method_procname cn ms kind in
  ignore (
    let proc_attributes =
      { (ProcAttributes.default (Procname.Java proc_name_java) Config.Java) with
        ProcAttributes.formals;
        ret_type = return_type;
      } in
    Cfg.Procdesc.create cfg proc_attributes)

(** returns the procedure description of the given method and creates it if it hasn't been created before *)
let rec get_method_procdesc program cfg tenv cn ms kind =
  let procname_java = JTransType.get_method_procname cn ms kind in
  match lookup_procdesc cfg (Procname.Java procname_java) with
  | Unknown ->
      create_external_procdesc program cfg tenv cn ms kind;
      get_method_procdesc program cfg tenv cn ms kind
  | Created status -> status

let use_static_final_fields context =
  (not !no_static_final) && (JContext.get_meth_kind context) <> JContext.Init

let builtin_new =
  Sil.Const (Sil.Cfun ModelBuiltins.__new)

let builtin_get_array_size =
  Sil.Const (Sil.Cfun ModelBuiltins.__get_array_size)

let create_sil_deref exp typ loc =
  let fresh_id = Ident.create_fresh Ident.knormal in
  let deref = Sil.Letderef (fresh_id, exp, typ, loc) in
  fresh_id, deref

(** translate an expression used as an r-value *)
let rec expression context pc expr =
  (* JUtils.log "\t\t\t\texpr: %s@." (JBir.print_expr expr); *)
  let cn = (JContext.get_cn context) in
  let program = JContext.get_program context in
  let loc = get_location (JContext.get_impl context) pc (JContext.get_meth_kind context) cn in
  let tenv = JContext.get_tenv context in
  let type_of_expr = JTransType.expr_type context expr in
  let trans_var pvar =
    let id = Ident.create_fresh Ident.knormal in
    let sil_instr = Sil.Letderef (id, Sil.Lvar pvar, type_of_expr, loc) in
    ([id], [sil_instr], Sil.Var id, type_of_expr) in
  match expr with
  | JBir.Var (_, var) ->
      let pvar = (JContext.set_pvar context var type_of_expr) in
      trans_var pvar
  | JBir.Const c ->
      begin
        match c with (* We use the constant <field> internally to mean a variable. *)
        | `String s when (JBasics.jstr_pp s) = JConfig.field_cst ->
            let varname = JConfig.field_st in
            let procname = (Cfg.Procdesc.get_proc_name (JContext.get_procdesc context)) in
            let pvar = Pvar.mk varname procname in
            trans_var pvar
        | _ -> ([], [], Sil.Const (get_constant c), type_of_expr)
      end
  | JBir.Unop (unop, ex) ->
      let type_of_ex = JTransType.expr_type context ex in
      let (ids, instrs, sil_ex, _) = expression context pc ex in
      begin
        match unop with
        | JBir.Neg _ -> (ids, instrs, Sil.UnOp (Sil.Neg, sil_ex, Some type_of_expr), type_of_expr)
        | JBir.ArrayLength ->
            let array_typ_no_ptr =
              match type_of_ex with
              | Sil.Tptr (typ, _) -> typ
              | _ -> type_of_ex in
            let fresh_id, deref = create_sil_deref sil_ex array_typ_no_ptr loc in
            let args = [(sil_ex, type_of_ex)] in
            let ret_id = Ident.create_fresh Ident.knormal in
            let call_instr = Sil.Call([ret_id], builtin_get_array_size, args, loc, Sil.cf_default) in
            (ids @ [fresh_id; ret_id], instrs @ [deref; call_instr], Sil.Var ret_id, type_of_expr)
        | JBir.Conv conv ->
            let cast_ex = Sil.Cast (JTransType.cast_type conv, sil_ex) in
            (ids, instrs, cast_ex, type_of_expr)
        | JBir.InstanceOf ot | JBir.Cast ot ->
            let subtypes =
              (match unop with
               | JBir.InstanceOf _ -> Sil.Subtype.subtypes_instof
               | JBir.Cast _ -> Sil.Subtype.subtypes_cast
               | _ -> assert false) in
            let sizeof_expr =
              JTransType.sizeof_of_object_type program tenv ot subtypes in
            let builtin =
              (match unop with
               | JBir.InstanceOf _ -> Sil.Const (Sil.Cfun ModelBuiltins.__instanceof)
               | JBir.Cast _ -> Sil.Const (Sil.Cfun ModelBuiltins.__cast)
               | _ -> assert false) in
            let args = [(sil_ex, type_of_ex); (sizeof_expr, Sil.Tvoid)] in
            let ret_id = Ident.create_fresh Ident.knormal in
            let call = Sil.Call([ret_id], builtin, args, loc, Sil.cf_default) in
            let res_ex = Sil.Var ret_id in
            (ids @ [ret_id], instrs @ [call], res_ex, type_of_expr)
      end
  | JBir.Binop (binop, ex1, ex2) ->
      let (idl1, instrs1, sil_ex1, _) = expression context pc ex1
      and (idl2, instrs2, sil_ex2, _) = expression context pc ex2 in
      begin
        match binop with
        | JBir.ArrayLoad _ ->
            (* add an instruction that dereferences the array *)
            let array_typ = Sil.Tarray(type_of_expr, Sil.Var (Ident.create_fresh Ident.kprimed)) in
            let fresh_id, deref_array_instr = create_sil_deref sil_ex1 array_typ loc in
            let id = Ident.create_fresh Ident.knormal in
            let letderef_instr =
              Sil.Letderef (id, Sil.Lindex (sil_ex1, sil_ex2), type_of_expr, loc) in
            let ids = idl1 @ idl2 @ [fresh_id; id] in
            let instrs = (instrs1 @ (deref_array_instr :: instrs2)) @ [letderef_instr] in
            ids, instrs, Sil.Var id, type_of_expr
        | other_binop ->
            let sil_binop = get_binop other_binop in
            let sil_expr = Sil.BinOp (sil_binop, sil_ex1, sil_ex2) in
            (idl1 @ idl2, (instrs1 @ instrs2), sil_expr, type_of_expr)
      end
  | JBir.Field (ex, cn, fs) ->
      let (idl, instrs, sil_expr, _) = expression context pc ex in
      let field_name = get_field_name program false tenv cn fs in
      let sil_type = JTransType.get_class_type_no_pointer program tenv cn in
      let sil_expr = Sil.Lfield (sil_expr, field_name, sil_type) in
      let tmp_id = Ident.create_fresh Ident.knormal in
      let lderef_instr = Sil.Letderef (tmp_id, sil_expr, sil_type, loc) in
      (idl @ [tmp_id], instrs @ [lderef_instr], Sil.Var tmp_id, type_of_expr)
  | JBir.StaticField (cn, fs) ->
      let class_exp =
        let classname = Mangled.from_string (JBasics.cn_name cn) in
        let var_name = Pvar.mk_global classname in
        Sil.Lvar var_name in
      let (idl, instrs, sil_expr) = [], [], class_exp in
      let field_name = get_field_name program true tenv cn fs in
      let sil_type = JTransType.get_class_type_no_pointer program tenv cn in
      if JTransStaticField.is_static_final_field context cn fs && use_static_final_fields context
      then
        (* when accessing a static final field, we call the initialiser method. *)
        let cfg = JContext.get_cfg context in
        let callee_procdesc =
          match get_method_procdesc program cfg tenv cn JBasics.clinit_signature Procname.Static with
          | Called p | Defined p -> p in
        let field_type =
          JTransType.get_class_type program tenv (JBasics.make_cn JConfig.string_cl) in
        let idl', instrs', expr' =
          JTransStaticField.translate_instr_static_field
            context callee_procdesc fs field_type loc in
        idl', instrs', expr', type_of_expr
      else
      if JTransType.is_autogenerated_assert_field field_name
      then
        (* assume that reading from C.$assertionsDisabled always yields "false". this allows *)
        (* Infer to understand the assert keyword in the expected way *)
        (idl, instrs, Sil.exp_zero, type_of_expr)
      else
        let sil_expr = Sil.Lfield (sil_expr, field_name, sil_type) in
        let tmp_id = Ident.create_fresh Ident.knormal in
        let lderef_instr = Sil.Letderef (tmp_id, sil_expr, sil_type, loc) in
        (idl @ [tmp_id], instrs @ [lderef_instr], Sil.Var tmp_id, type_of_expr)

let method_invocation context loc pc var_opt cn ms sil_obj_opt expr_list invoke_code method_kind =
  (* This function tries to recursively search for the classname of the class *)
  (* where the method is defined. It returns the classname given as argument*)
  (* when this classname cannot be found *)
  let resolve_method context cn ms =
    let rec loop fallback_cn cn =
      match JClasspath.lookup_node cn (JContext.get_program context) with
      | None -> fallback_cn
      | Some node ->
          if Javalib.defines_method node ms then cn
          else
            match node with
            | Javalib.JInterface _ -> fallback_cn
            | Javalib.JClass jclass ->
                begin
                  match jclass.Javalib.c_super_class with
                  | None -> fallback_cn
                  | Some super_cn -> loop fallback_cn super_cn
                end in
    loop cn cn in
  let cn' = resolve_method context cn ms in
  let tenv = JContext.get_tenv context in
  let program = JContext.get_program context in
  let cf_virtual, cf_interface = match invoke_code with
    | I_Virtual -> (true, false)
    | I_Interface -> (true, true)
    | _ -> (false, false) in
  let call_flags =
    { Sil.cf_default with Sil.cf_virtual = cf_virtual; Sil.cf_interface = cf_interface; } in
  let init =
    match sil_obj_opt with
    | None -> ([], [], [])
    | Some (sil_obj_expr, sil_obj_type) ->
        (* for non-constructors, add an instruction that dereferences the receiver *)
        let ids, instrs =
          let is_non_constructor_call =
            match invoke_code with
            | I_Special -> false
            | _ -> true in
          match sil_obj_expr with
          | Sil.Var _ when is_non_constructor_call && not !JConfig.translate_checks ->
              let obj_typ_no_ptr =
                match sil_obj_type with
                | Sil.Tptr (typ, _) -> typ
                | _ -> sil_obj_type in
              let fresh_id, deref = create_sil_deref sil_obj_expr obj_typ_no_ptr loc in
              [fresh_id], [deref]
          | _ -> [], [] in
        (ids, instrs, [(sil_obj_expr, sil_obj_type)]) in
  let (idl, instrs, call_args) =
    IList.fold_left
      (fun (idl_accu, instrs_accu, args_accu) expr ->
         let (idl, instrs, sil_expr, sil_expr_type) = expression context pc expr in
         (idl_accu @ idl, instrs_accu @ instrs, args_accu @ [(sil_expr, sil_expr_type)]))
      init
      expr_list in
  let callee_procname =
    let proc = Procname.from_string_c_fun (JBasics.ms_name ms) in
    if JBasics.cn_equal cn' JConfig.infer_builtins_cl &&
       Builtin.is_registered proc
    then proc
    else Procname.Java (JTransType.get_method_procname cn' ms method_kind) in
  let call_idl, call_instrs =
    let callee_fun = Sil.Const (Sil.Cfun callee_procname) in
    let return_type =
      match JBasics.ms_rtype ms with
      | None -> Sil.Tvoid
      | Some vt -> JTransType.value_type program tenv vt in
    let call_ret_instrs sil_var =
      let ret_id = Ident.create_fresh Ident.knormal in
      let call_instr = Sil.Call ([ret_id], callee_fun, call_args, loc, call_flags) in
      let set_instr = Sil.Set (Sil.Lvar sil_var, return_type, Sil.Var ret_id, loc) in
      (idl @ [ret_id], instrs @ [call_instr; set_instr]) in
    match var_opt with
    | None ->
        let call_instr = Sil.Call ([], callee_fun, call_args, loc, call_flags) in
        (idl, instrs @ [call_instr])
    | Some var ->
        let sil_var = JContext.set_pvar context var return_type in
        (call_ret_instrs sil_var) in
  let instrs =
    match call_args with
    (* modeling a class bypasses the treatment of Closeable *)
    | _ when Config.analyze_models || JClasspath.is_model callee_procname -> call_instrs

    (* add a file attribute when calling the constructor of a subtype of Closeable *)
    | (_, typ) as exp :: _
      when Procname.is_constructor callee_procname && JTransType.is_closeable program tenv typ ->
        let set_file_attr =
          let set_builtin = Sil.Const (Sil.Cfun ModelBuiltins.__set_file_attribute) in
          Sil.Call ([], set_builtin, [exp], loc, Sil.cf_default) in
        (* Exceptions thrown in the constructor should prevent adding the resource attribute *)
        call_instrs @ [set_file_attr]

    (* remove file attribute when calling the close method of a subtype of Closeable *)
    | (_, typ) as exp :: []
      when Procname.java_is_close callee_procname && JTransType.is_closeable program tenv typ ->
        let set_mem_attr =
          let set_builtin = Sil.Const (Sil.Cfun ModelBuiltins.__set_mem_attribute) in
          Sil.Call ([], set_builtin, [exp], loc, Sil.cf_default) in
        (* Exceptions thrown in the close method should not prevent the resource from being *)
        (* considered as closed *)
        [set_mem_attr] @ call_instrs

    | _ -> call_instrs in

  (callee_procname, call_idl, instrs)

let get_array_size context pc expr_list content_type =
  let get_expr_instr expr other_instrs =
    let (idl, instrs, sil_size_expr, _) = expression context pc expr in
    match other_instrs with
    | (other_idl, other_instrs, other_exprs) ->
        (idl@other_idl, instrs@other_instrs, sil_size_expr:: other_exprs) in
  let (idl, instrs, sil_size_exprs) = (IList.fold_right get_expr_instr expr_list ([],[],[])) in
  let get_array_type sil_size_expr content_type =
    Sil.Tarray (content_type, sil_size_expr) in
  let array_type = (IList.fold_right get_array_type sil_size_exprs content_type) in
  let array_size = Sil.Sizeof (array_type, Sil.Subtype.exact) in
  (idl, instrs, array_size)

module Int =
struct
  type t = int
  let compare = (-)
end

module IntSet = Set.Make(Int)

let detect_loop entry_pc impl =
  let code = (JBir.code impl) in
  let pc_bound = Array.length code in
  let empty = IntSet.empty in
  let rec loop visited pc =
    if (IntSet.mem pc visited) || pc >= pc_bound then
      (false, visited)
    else
      begin
        let visited_updated = IntSet.add pc visited in
        match code.(pc) with
        | JBir.Goto goto_pc when goto_pc = entry_pc -> (true, empty)
        | JBir.Goto goto_pc -> loop visited_updated goto_pc
        | JBir.Ifd (_, if_pc) when if_pc = entry_pc -> (true, empty)
        | JBir.Ifd (_, if_pc) ->
            let (loop_detected, visited_after) = loop visited_updated (pc + 1) in
            if loop_detected then
              (true, empty)
            else
              loop visited_after if_pc
        | _ ->
            if (pc + 1) = entry_pc then
              (true, empty)
            else
              loop visited_updated (pc + 1)
      end in
  fst (loop empty entry_pc)

type translation =
  | Skip
  | Instr of Cfg.Node.t
  | Prune of Cfg.Node.t * Cfg.Node.t
  | Loop of Cfg.Node.t * Cfg.Node.t * Cfg.Node.t

(* TODO: unclear if this corresponds to what JControlFlow.resolve_method'*)
(* is trying to do. Normally, this implementation below goes deeper into *)
(* the type hierarchy and it is not clear why we should not do that *)
let extends context node1 node2 =
  let is_matching cn =
    JBasics.cn_equal cn (Javalib.get_name node2) in
  let rec check cn_list =
    if IList.exists is_matching cn_list then true
    else
      iterate cn_list
  and iterate cn_list =
    let per_classname cn =
      match JClasspath.lookup_node cn (JContext.get_program context) with
      | None -> false (* TODO: should capture the class instead of returning false *)
      | Some node ->
          let super_cn_list =
            match node with
            | Javalib.JInterface jinterface ->
                jinterface.Javalib.i_interfaces
            | Javalib.JClass jclass ->
                let cn_interfaces = jclass.Javalib.c_interfaces in
                begin
                  match jclass.Javalib.c_super_class with
                  | None -> cn_interfaces
                  | Some super_cn -> super_cn :: cn_interfaces
                end in
          match super_cn_list with
          | [] -> false
          | l -> check l in
    IList.exists per_classname cn_list in
  check [Javalib.get_name node1]

let instruction_array_call ms obj_type obj args var_opt =
  if is_clone ms then
    (let cn = JBasics.make_cn JConfig.infer_array_cl in
     let vt = (JBasics.TObject obj_type) in
     let ms = JBasics.make_ms JConfig.clone_name [vt] (Some vt) in
     JBir.InvokeStatic (var_opt, cn, ms, obj:: args))
  else
    (let undef_cn, undef_ms = get_undefined_method_call (JBasics.ms_rtype ms) in
     JBir.InvokeStatic (var_opt, undef_cn, undef_ms, []))

(* special translation of the method start() of a Thread or a Runnable object.
   We translate it directly as the run() method *)
let instruction_thread_start context cn ms obj args var_opt =
  match JClasspath.lookup_node cn (JContext.get_program context) with
  | None ->
      let () = JUtils.log "\t\t\tWARNING: %s should normally be found@." (JBasics.cn_name cn) in
      None
  | Some node ->
      begin
        match JClasspath.lookup_node (JBasics.make_cn JConfig.thread_class) (JContext.get_program context) with
        | None -> None (* TODO: should load the class instead of returning None *)
        | Some thread_node ->
            if ((JBasics.ms_name ms) = JConfig.start_method) && (extends context node thread_node) then
              let ms = JBasics.make_ms JConfig.run_method [] None in
              Some (JBir.InvokeNonVirtual (var_opt, obj, cn, ms, args))
            else None
      end


let is_this expr =
  match expr with
  | JBir.Var (_, var) ->
      begin
        match JBir.var_name_debug var with
        | None -> false
        | Some name_opt -> Mangled.to_string JConfig.this = name_opt
      end
  | _ -> false


let assume_not_null loc sil_expr =
  let builtin_infer_assume = Sil.Const (Sil.Cfun ModelBuiltins.__infer_assume) in
  let not_null_expr =
    Sil.BinOp (Sil.Ne, sil_expr, Sil.exp_null) in
  let assume_call_flag = { Sil.cf_default with Sil.cf_noreturn = true; } in
  let call_args = [(not_null_expr, Sil.Tint Sil.IBool)] in
  Sil.Call ([], builtin_infer_assume, call_args, loc, assume_call_flag)

let rec instruction context pc instr : translation =
  let cfg = JContext.get_cfg context in
  let tenv = JContext.get_tenv context in
  let cg = JContext.get_cg context in
  let cn = JContext.get_cn context in
  let program = JContext.get_program context in
  let meth_kind = JContext.get_meth_kind context in
  let proc_name = Cfg.Procdesc.get_proc_name (JContext.get_procdesc context) in
  let ret_var = Pvar.get_ret_pvar proc_name in
  let ret_type = Cfg.Procdesc.get_ret_type (JContext.get_procdesc context) in
  let loc = get_location (JContext.get_impl context) pc meth_kind cn in
  let match_never_null = JContext.get_never_null_matcher context in
  let create_node node_kind temps sil_instrs =
    Cfg.Node.create
      cfg (get_location (JContext.get_impl context) pc meth_kind cn) node_kind sil_instrs (JContext.get_procdesc context) temps in
  let return_not_null () =
    match_never_null loc.Location.file proc_name
    ||
    IList.exists
      (fun pnj -> Procname.equal (Procname.Java pnj) proc_name)
      JTransType.never_returning_null in
  let trans_monitor_enter_exit context expr pc loc builtin node_desc =
    let ids, instrs, sil_expr, sil_type = expression context pc expr in
    let builtin_const = Sil.Const (Sil.Cfun builtin) in
    let instr = Sil.Call ([], builtin_const, [(sil_expr, sil_type)], loc, Sil.cf_default) in
    let node_kind = Cfg.Node.Stmt_node node_desc in
    Instr (create_node node_kind ids (instrs @ [instr])) in
  try
    match instr with
    | JBir.AffectVar (var, expr) ->
        let (idl, stml, sil_expr, sil_type) = expression context pc expr in
        let pvar = (JContext.set_pvar context var sil_type) in
        let sil_instr = Sil.Set (Sil.Lvar pvar, sil_type, sil_expr, loc) in
        let node_kind = Cfg.Node.Stmt_node "method_body" in
        let node = create_node node_kind idl (stml @ [sil_instr]) in
        Instr node
    | JBir.Return expr_option ->
        let node_kind = Cfg.Node.Stmt_node "method_body" in
        let node =
          match expr_option with
          | None ->
              create_node node_kind [] []
          | Some expr ->
              let (idl, stml, sil_expr, _) = expression context pc expr in
              let sil_instrs =
                let return_instr = Sil.Set (Sil.Lvar ret_var, ret_type, sil_expr, loc) in
                if return_not_null () then
                  [assume_not_null loc sil_expr; return_instr]
                else
                  [return_instr] in
              create_node node_kind idl (stml @ sil_instrs) in
        JContext.add_goto_jump context pc JContext.Exit;
        Instr node
    | JBir.AffectArray (array_ex, index_ex, value_ex) ->
        let (idl_array, instrs_array, sil_expr_array, arr_type) = expression context pc array_ex
        and (idl_index, instrs_index, sil_expr_index, _) = expression context pc index_ex
        and (idl_value, instrs_value, sil_expr_value, _) = expression context pc value_ex in
        let arr_type_np = JTransType.extract_cn_type_np arr_type in
        let sil_instr = Sil.Set (Sil.Lindex (sil_expr_array, sil_expr_index), arr_type_np, sil_expr_value, loc) in
        let final_idl = idl_array @ idl_index @ idl_value
        and final_instrs = instrs_array @ instrs_index @ instrs_value @ [sil_instr] in
        let node_kind = Cfg.Node.Stmt_node "method_body" in
        let node = create_node node_kind final_idl final_instrs in
        Instr node
    | JBir.AffectField (e_lhs, cn, fs, e_rhs) ->
        let (idl1, stml1, sil_expr_lhs, _) = expression context pc e_lhs in
        let (idl2, stml2, sil_expr_rhs, _) = expression context pc e_rhs in
        let field_name = get_field_name program false tenv cn fs in
        let type_of_the_surrounding_class = JTransType.get_class_type_no_pointer program tenv cn in
        let type_of_the_root_of_e_lhs = type_of_the_surrounding_class in
        let expr_off = Sil.Lfield(sil_expr_lhs, field_name, type_of_the_surrounding_class) in
        let sil_instr = Sil.Set (expr_off, type_of_the_root_of_e_lhs, sil_expr_rhs, loc) in
        let node_kind = Cfg.Node.Stmt_node "method_body" in
        let node = create_node node_kind (idl1 @ idl2) (stml1 @ stml2 @ [sil_instr]) in
        Instr node
    | JBir.AffectStaticField (cn, fs, e_rhs) ->
        let class_exp =
          let classname = Mangled.from_string (JBasics.cn_name cn) in
          let var_name = Pvar.mk_global classname in
          Sil.Lvar var_name in
        let (idl1, stml1, sil_expr_lhs) = [], [], class_exp in
        let (idl2, stml2, sil_expr_rhs, _) = expression context pc e_rhs in
        let field_name = get_field_name program true tenv cn fs in
        let type_of_the_surrounding_class = JTransType.get_class_type_no_pointer program tenv cn in
        let type_of_the_root_of_e_lhs = type_of_the_surrounding_class in
        let expr_off = Sil.Lfield(sil_expr_lhs, field_name, type_of_the_surrounding_class) in
        let sil_instr = Sil.Set (expr_off, type_of_the_root_of_e_lhs, sil_expr_rhs, loc) in
        let node_kind = Cfg.Node.Stmt_node "method_body" in
        let node = create_node node_kind (idl1 @ idl2) (stml1 @ stml2 @ [sil_instr]) in
        Instr node
    | JBir.Goto goto_pc ->
        JContext.reset_pvar_type context;
        JContext.add_goto_jump context pc (JContext.Jump goto_pc);
        Skip
    | JBir.Ifd ((op, e1, e2), if_pc) -> (* Note: JBir provides the condition for the false branch, under which to jump *)
        JContext.reset_pvar_type context;
        let (idl1, instrs1, sil_ex1, _) = expression context pc e1
        and (idl2, instrs2, sil_ex2, _) = expression context pc e2 in
        let sil_op = get_test_operator op in
        let sil_test_false = Sil.BinOp (sil_op, sil_ex1, sil_ex2) in
        let sil_test_true = Sil.UnOp(Sil.LNot, sil_test_false, None) in
        let sil_instrs_true = Sil.Prune (sil_test_true, loc, true, Sil.Ik_if) in
        let sil_instrs_false = Sil.Prune (sil_test_false, loc, false, Sil.Ik_if) in
        let node_kind_true = Cfg.Node.Prune_node (true, Sil.Ik_if, "method_body") in
        let node_kind_false = Cfg.Node.Prune_node (false, Sil.Ik_if, "method_body") in
        let prune_node_true = create_node node_kind_true (idl1 @ idl2) (instrs1 @ instrs2 @ [sil_instrs_true])
        and prune_node_false = create_node node_kind_false (idl1 @ idl2) (instrs1 @ instrs2 @ [sil_instrs_false]) in
        JContext.add_if_jump context prune_node_false if_pc;
        if detect_loop pc (JContext.get_impl context) then
          let join_node_kind = Cfg.Node.Join_node in
          let join_node = create_node join_node_kind [] [] in
          Loop (join_node, prune_node_true, prune_node_false)
        else
          Prune (prune_node_true, prune_node_false)
    | JBir.Throw expr ->
        let (ids, instrs, sil_expr, _) = expression context pc expr in
        let sil_exn = Sil.Const (Sil.Cexn sil_expr) in
        let sil_instr = Sil.Set (Sil.Lvar ret_var, ret_type, sil_exn, loc) in
        let node = create_node Cfg.Node.throw_kind ids (instrs @ [sil_instr]) in
        JContext.add_goto_jump context pc JContext.Exit;
        Instr node
    | JBir.New (var, cn, constr_type_list, constr_arg_list) ->
        let builtin_new = Sil.Const (Sil.Cfun ModelBuiltins.__new) in
        let class_type = JTransType.get_class_type program tenv cn in
        let class_type_np = JTransType.get_class_type_no_pointer program tenv cn in
        let sizeof_exp = Sil.Sizeof (class_type_np, Sil.Subtype.exact) in
        let args = [(sizeof_exp, class_type)] in
        let ret_id = Ident.create_fresh Ident.knormal in
        let new_instr = Sil.Call([ret_id], builtin_new, args, loc, Sil.cf_default) in
        let constr_ms = JBasics.make_ms JConfig.constructor_name constr_type_list None in
        let constr_procname, call_ids, call_instrs =
          let ret_opt = Some (Sil.Var ret_id, class_type) in
          method_invocation
            context loc pc None cn constr_ms ret_opt constr_arg_list I_Special Procname.Non_Static in
        let pvar = JContext.set_pvar context var class_type in
        let set_instr = Sil.Set (Sil.Lvar pvar, class_type, Sil.Var ret_id, loc) in
        let ids = ret_id :: call_ids in
        let instrs = (new_instr :: call_instrs) @ [set_instr] in
        let node_kind = Cfg.Node.Stmt_node ("Call "^(Procname.to_string constr_procname)) in
        let node = create_node node_kind ids instrs in
        let caller_procname = (Cfg.Procdesc.get_proc_name (JContext.get_procdesc context)) in
        Cg.add_edge cg caller_procname constr_procname;
        Instr node
    | JBir.NewArray (var, vt, expr_list) ->
        let builtin_new_array = Sil.Const (Sil.Cfun ModelBuiltins.__new_array) in
        let content_type = JTransType.value_type program tenv vt in
        let array_type = JTransType.create_array_type content_type (IList.length expr_list) in
        let array_name = JContext.set_pvar context var array_type in
        let (idl, instrs, array_size) = get_array_size context pc expr_list content_type in
        let call_args = [(array_size, array_type)] in
        let ret_id = Ident.create_fresh Ident.knormal in
        let call_instr = Sil.Call([ret_id], builtin_new_array, call_args, loc, Sil.cf_default) in
        let set_instr = Sil.Set (Sil.Lvar array_name, array_type, Sil.Var ret_id, loc) in
        let node_kind = Cfg.Node.Stmt_node "method_body" in
        let node = create_node node_kind (idl @ [ret_id]) (instrs @ [call_instr; set_instr]) in
        Instr node
    | JBir.InvokeStatic (var_opt, cn, ms, args) ->
        let sil_obj_opt, args, ids, instrs =
          match args with
          | [arg] when is_clone ms ->
              (* hack to null check the receiver of clone when clone is an array. in the array.clone()
                 case, clone is a virtual call that we translate as a static call *)
              let (ids, instrs, sil_arg_expr, arg_typ) = expression context pc arg in
              Some (sil_arg_expr, arg_typ), [], ids, instrs
          | _ -> None, args, [], [] in
        let callee_procname, call_idl, call_instrs =
          method_invocation context loc pc var_opt cn ms sil_obj_opt args I_Static Procname.Static in
        let node_kind = Cfg.Node.Stmt_node ("Call "^(Procname.to_string callee_procname)) in
        let call_node = create_node node_kind (ids @ call_idl) (instrs @ call_instrs) in
        let caller_procname = (Cfg.Procdesc.get_proc_name (JContext.get_procdesc context)) in
        Cg.add_edge cg caller_procname callee_procname;
        Instr call_node
    | JBir.InvokeVirtual (var_opt, obj, call_kind, ms, args) ->
        let caller_procname = (Cfg.Procdesc.get_proc_name (JContext.get_procdesc context)) in
        let (ids, instrs, sil_obj_expr, sil_obj_type) = expression context pc obj in
        let create_call_node cn invoke_kind =
          let callee_procname, call_ids, call_instrs =
            let ret_opt = Some (sil_obj_expr, sil_obj_type) in
            method_invocation
              context loc pc var_opt cn ms ret_opt args invoke_kind Procname.Non_Static in
          let node_kind = Cfg.Node.Stmt_node ("Call "^(Procname.to_string callee_procname)) in
          let call_node = create_node node_kind (ids @ call_ids) (instrs @ call_instrs) in
          Cg.add_edge cg caller_procname callee_procname;
          call_node in
        let trans_virtual_call original_cn invoke_kind =
          match instruction_thread_start context original_cn ms obj args var_opt with
          | Some start_call -> instruction context pc start_call
          | None ->
              let cn' = match JTransType.extract_cn_no_obj sil_obj_type with
                | Some cn -> cn
                | None -> original_cn in
              let call_node = create_call_node cn' invoke_kind in
              Instr call_node in
        begin
          match call_kind with
          | JBir.VirtualCall obj_type ->
              begin
                match obj_type with
                | JBasics.TClass cn -> trans_virtual_call cn I_Virtual
                | JBasics.TArray _ ->
                    let instr = instruction_array_call ms obj_type obj args var_opt in
                    instruction context pc instr
              end
          | JBir.InterfaceCall cn ->
              trans_virtual_call cn I_Interface
        end
    | JBir.InvokeNonVirtual (var_opt, obj, cn, ms, args) ->
        let (ids, instrs, sil_obj_expr, sil_obj_type) = expression context pc obj in
        let callee_procname, call_ids, call_instrs =
          method_invocation context loc pc var_opt cn ms (Some (sil_obj_expr, sil_obj_type)) args I_Special Procname.Non_Static in
        let node_kind = Cfg.Node.Stmt_node ("Call "^(Procname.to_string callee_procname)) in
        let call_node = create_node node_kind (ids @ call_ids) (instrs @ call_instrs) in
        let procdesc = (JContext.get_procdesc context) in
        let caller_procname = (Cfg.Procdesc.get_proc_name procdesc) in
        Cg.add_edge cg caller_procname callee_procname;
        Instr call_node

    | JBir.Check (JBir.CheckNullPointer expr) when !JConfig.translate_checks && is_this expr ->
        (* TODO #6509339: refactor the boilterplate code in the translattion of JVM checks *)
        let (ids, instrs, sil_expr, _) = expression context pc expr in
        let this_not_null_node =
          create_node
            (Cfg.Node.Stmt_node "this not null") ids (instrs @ [assume_not_null loc sil_expr]) in
        Instr this_not_null_node

    | JBir.Check (JBir.CheckNullPointer expr) when !JConfig.translate_checks ->
        let (ids, instrs, sil_expr, _) = expression context pc expr in
        let not_null_node =
          let sil_not_null = Sil.BinOp (Sil.Ne, sil_expr, Sil.exp_null) in
          let sil_prune_not_null = Sil.Prune (sil_not_null, loc, true, Sil.Ik_if)
          and not_null_kind = Cfg.Node.Prune_node (true, Sil.Ik_if, "Not null") in
          create_node not_null_kind ids (instrs @ [sil_prune_not_null]) in
        let throw_npe_node =
          let sil_is_null = Sil.BinOp (Sil.Eq, sil_expr, Sil.exp_null) in
          let sil_prune_null = Sil.Prune (sil_is_null, loc, true, Sil.Ik_if)
          and npe_kind = Cfg.Node.Stmt_node "Throw NPE"
          and npe_cn = JBasics.make_cn JConfig.npe_cl in
          let class_type = JTransType.get_class_type program tenv npe_cn
          and class_type_np = JTransType.get_class_type_no_pointer program tenv npe_cn in
          let sizeof_exp = Sil.Sizeof (class_type_np, Sil.Subtype.exact) in
          let args = [(sizeof_exp, class_type)] in
          let ret_id = Ident.create_fresh Ident.knormal in
          let new_instr = Sil.Call([ret_id], builtin_new, args, loc, Sil.cf_default) in
          let constr_ms = JBasics.make_ms JConfig.constructor_name [] None in
          let _, call_ids, call_instrs =
            let ret_opt = Some (Sil.Var ret_id, class_type) in
            method_invocation context loc pc None npe_cn constr_ms ret_opt [] I_Special Procname.Static in
          let sil_exn = Sil.Const (Sil.Cexn (Sil.Var ret_id)) in
          let set_instr = Sil.Set (Sil.Lvar ret_var, ret_type, sil_exn, loc) in
          let npe_instrs = instrs @ [sil_prune_null] @ (new_instr :: call_instrs) @ [set_instr] in
          create_node npe_kind (ids @ call_ids) npe_instrs in
        Prune (not_null_node, throw_npe_node)

    | JBir.Check (JBir.CheckArrayBound (array_expr, index_expr)) when !JConfig.translate_checks ->

        let ids, instrs, _, sil_length_expr, sil_index_expr =
          let array_ids, array_instrs, sil_array_expr, _ =
            expression context pc array_expr
          and length_ids, length_instrs, sil_length_expr, _ =
            expression context pc (JBir.Unop (JBir.ArrayLength, array_expr))
          and index_ids, index_instrs, sil_index_expr, _ =
            expression context pc index_expr in
          let ids = array_ids @ index_ids @ length_ids
          and instrs = array_instrs @ index_instrs @ length_instrs in
          (ids, instrs, sil_array_expr, sil_length_expr, sil_index_expr) in

        let in_bound_node =
          let in_bound_node_kind =
            Cfg.Node.Prune_node (true, Sil.Ik_if, "In bound") in
          let sil_assume_in_bound =
            let sil_in_bound =
              let sil_positive_index =
                Sil.BinOp (Sil.Ge, sil_index_expr, Sil.Const (Sil.Cint Sil.Int.zero))
              and sil_less_than_length =
                Sil.BinOp (Sil.Lt, sil_index_expr, sil_length_expr) in
              Sil.BinOp (Sil.LAnd, sil_positive_index, sil_less_than_length) in
            Sil.Prune (sil_in_bound, loc, true, Sil.Ik_if) in
          create_node in_bound_node_kind ids (instrs @ [sil_assume_in_bound])

        and throw_out_of_bound_node =
          let out_of_bound_node_kind =
            Cfg.Node.Stmt_node "Out of bound" in
          let sil_assume_out_of_bound =
            let sil_out_of_bound =
              let sil_negative_index =
                Sil.BinOp (Sil.Lt, sil_index_expr, Sil.Const (Sil.Cint Sil.Int.zero))
              and sil_greater_than_length =
                Sil.BinOp (Sil.Gt, sil_index_expr, sil_length_expr) in
              Sil.BinOp (Sil.LOr, sil_negative_index, sil_greater_than_length) in
            Sil.Prune (sil_out_of_bound, loc, true, Sil.Ik_if) in
          let out_of_bound_cn = JBasics.make_cn JConfig.out_of_bound_cl in
          let class_type = JTransType.get_class_type program tenv out_of_bound_cn
          and class_type_np = JTransType.get_class_type_no_pointer program tenv out_of_bound_cn in
          let sizeof_exp = Sil.Sizeof (class_type_np, Sil.Subtype.exact) in
          let args = [(sizeof_exp, class_type)] in
          let ret_id = Ident.create_fresh Ident.knormal in
          let new_instr = Sil.Call([ret_id], builtin_new, args, loc, Sil.cf_default) in
          let constr_ms = JBasics.make_ms JConfig.constructor_name [] None in
          let _, call_ids, call_instrs =
            method_invocation
              context loc pc None out_of_bound_cn constr_ms
              (Some (Sil.Var ret_id, class_type)) [] I_Special Procname.Static in
          let sil_exn = Sil.Const (Sil.Cexn (Sil.Var ret_id)) in
          let set_instr = Sil.Set (Sil.Lvar ret_var, ret_type, sil_exn, loc) in
          let out_of_bound_instrs =
            instrs @ [sil_assume_out_of_bound] @ (new_instr :: call_instrs) @ [set_instr] in
          create_node out_of_bound_node_kind (ids @ call_ids) out_of_bound_instrs in

        Prune (in_bound_node, throw_out_of_bound_node)

    | JBir.Check (JBir.CheckCast (expr, object_type)) when !JConfig.translate_checks ->
        let sil_type = JTransType.expr_type context expr
        and ids, instrs, sil_expr, _ = expression context pc expr
        and ret_id = Ident.create_fresh Ident.knormal
        and sizeof_expr =
          JTransType.sizeof_of_object_type program tenv object_type Sil.Subtype.subtypes_instof in
        let check_cast = Sil.Const (Sil.Cfun ModelBuiltins.__instanceof) in
        let args = [(sil_expr, sil_type); (sizeof_expr, Sil.Tvoid)] in
        let call = Sil.Call([ret_id], check_cast, args, loc, Sil.cf_default) in
        let res_ex = Sil.Var ret_id in
        let is_instance_node =
          let check_is_false = Sil.BinOp (Sil.Ne, res_ex, Sil.exp_zero) in
          let asssume_instance_of = Sil.Prune (check_is_false, loc, true, Sil.Ik_if)
          and instance_of_kind = Cfg.Node.Prune_node (true, Sil.Ik_if, "Is instance") in
          create_node instance_of_kind ids (instrs @ [call; asssume_instance_of])
        and throw_cast_exception_node =
          let check_is_true = Sil.BinOp (Sil.Ne, res_ex, Sil.exp_one) in
          let asssume_not_instance_of = Sil.Prune (check_is_true, loc, true, Sil.Ik_if)
          and throw_cast_exception_kind = Cfg.Node.Stmt_node "Class cast exception"
          and cce_cn = JBasics.make_cn JConfig.cce_cl in
          let class_type = JTransType.get_class_type program tenv cce_cn
          and class_type_np = JTransType.get_class_type_no_pointer program tenv cce_cn in
          let sizeof_exp = Sil.Sizeof (class_type_np, Sil.Subtype.exact) in
          let args = [(sizeof_exp, class_type)] in
          let ret_id = Ident.create_fresh Ident.knormal in
          let new_instr = Sil.Call([ret_id], builtin_new, args, loc, Sil.cf_default) in
          let constr_ms = JBasics.make_ms JConfig.constructor_name [] None in
          let _, call_ids, call_instrs =
            method_invocation context loc pc None cce_cn constr_ms
              (Some (Sil.Var ret_id, class_type)) [] I_Special Procname.Static in
          let sil_exn = Sil.Const (Sil.Cexn (Sil.Var ret_id)) in
          let set_instr = Sil.Set (Sil.Lvar ret_var, ret_type, sil_exn, loc) in
          let cce_instrs =
            instrs @ [call; asssume_not_instance_of] @ (new_instr :: call_instrs) @ [set_instr] in
          create_node throw_cast_exception_kind (ids @ call_ids) cce_instrs in

        Prune (is_instance_node, throw_cast_exception_node)
    | JBir.MonitorEnter expr ->
        trans_monitor_enter_exit
          context expr pc loc ModelBuiltins.__set_locked_attribute "MonitorEnter"

    | JBir.MonitorExit expr ->
        trans_monitor_enter_exit
          context expr pc loc ModelBuiltins.__set_unlocked_attribute "MonitorExit"

    | _ -> Skip
  with Frontend_error s ->
    JUtils.log "Skipping because of: %s@." s;
    Skip

(*
let static_field_name cn fs =
  let classname = JBasics.cn_name cn in
  let fieldname = JBasics.fs_name fs in
  Mangled.from_string (classname^"."^fieldname)
*)
