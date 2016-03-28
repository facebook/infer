(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open LAst

exception ImproperTypeError of string
exception MalformedMetadata of string
exception Unimplemented of string

let source_only_location () : Location.t =
  let open Location in { line = -1; col = -1; file = !DB.current_source; nLOC = !Config.nLOC }

let ident_of_variable (var : LAst.variable) : Ident.t = (* TODO: use unique stamps *)
  Ident.create_normal (Ident.string_to_name (LAst.string_of_variable var)) 0

let trans_variable (var : LAst.variable) : Sil.exp = Sil.Var (ident_of_variable var)

let trans_constant : LAst.constant -> Sil.exp = function
  | Cint i -> Sil.Const (Sil.Cint (Sil.Int.of_int i))
  | Cnull -> Sil.exp_null

let trans_operand : LAst.operand -> Sil.exp = function
  | Var var -> trans_variable var
  | Const const -> trans_constant const

let rec trans_typ : LAst.typ -> Sil.typ = function
  | Tint _i -> Sil.Tint Sil.IInt (* TODO: check what size int is needed here *)
  | Tfloat -> Sil.Tfloat Sil.FFloat
  | Tptr tp -> Sil.Tptr (trans_typ tp, Sil.Pk_pointer)
  | Tvector (i, tp)
  | Tarray (i, tp) -> Sil.Tarray (trans_typ tp, Sil.Const (Sil.Cint (Sil.Int.of_int i)))
  | Tfunc _ -> Sil.Tfun false
  | Tlabel -> raise (ImproperTypeError "Tried to generate Sil type from LLVM label type.")
  | Tmetadata -> raise (ImproperTypeError "Tried to generate Sil type from LLVM metadata type.")

let location_of_annotation_option (metadata : LAst.metadata_map)
  : LAst.annotation option -> Location.t = function
  | None -> source_only_location () (* no source line/column numbers *)
  | Some (Annotation i) ->
      begin match MetadataMap.find i metadata with
        | Components (TypOperand (_, Const (Cint line_num)) :: _) ->
            let open Location in
            { line = line_num; col = -1; file = !DB.current_source; nLOC = !Config.nLOC }
        | Location loc ->
            let open Location in
            { line = loc.line; col = loc.col; file = !DB.current_source; nLOC = !Config.nLOC }
        | _ -> raise (MalformedMetadata ("Instruction annotation refers to metadata node " ^
                                         "without line number as first component."))
      end

let procname_of_function_variable (func_var : LAst.variable) : Procname.t =
  Procname.from_string_c_fun (LAst.string_of_variable func_var)

(* Generate list of SIL instructions and list of local variables *)
let rec trans_annotated_instructions
    (cfg : Cfg.cfg) (procdesc : Cfg.Procdesc.t) (metadata : LAst.metadata_map)
  : LAst.annotated_instruction list -> Sil.instr list * (Mangled.t * Sil.typ) list = function
  | [] -> ([], [])
  | (instr, anno) :: t ->
      let (sil_instrs, locals) = trans_annotated_instructions cfg procdesc metadata t in
      let location = location_of_annotation_option metadata anno in
      begin match instr with
        | Ret None -> (sil_instrs, locals)
        | Ret (Some (tp, exp)) ->
            let procname = Cfg.Procdesc.get_proc_name procdesc in
            let ret_var = Pvar.get_ret_pvar procname in
            let new_sil_instr =
              Sil.Set (Sil.Lvar ret_var, trans_typ tp, trans_operand exp, location) in
            (new_sil_instr :: sil_instrs, locals)
        | Load (var, tp, ptr) ->
            let new_sil_instr =
              Sil.Letderef (ident_of_variable var, trans_variable ptr, trans_typ tp, location) in
            (new_sil_instr :: sil_instrs, locals)
        | Store (op, tp, var) ->
            let new_sil_instr =
              Sil.Set (trans_variable var, trans_typ tp, trans_operand op, location) in
            (new_sil_instr :: sil_instrs, locals)
        | Alloc (var, tp, _num_elems) ->
            (* num_elems currently ignored *)
            begin match var with
              | Global (Name var_name) | Local (Name var_name) ->
                  let new_local = (Mangled.from_string var_name, trans_typ (Tptr tp)) in
                  (sil_instrs, new_local :: locals)
              | _ ->
                  raise
                    (ImproperTypeError
                       "Not expecting alloca instruction to a numbered variable.")
            end
        | Call (ret_var, func_var, typed_args) ->
            let new_sil_instr = Sil.Call (
                [ident_of_variable ret_var],
                Sil.Const (Sil.Cfun (procname_of_function_variable func_var)),
                IList.map (fun (tp, arg) -> (trans_operand arg, trans_typ tp)) typed_args,
                location, Sil.cf_default) in
            (new_sil_instr :: sil_instrs, locals)
        | _ -> raise (Unimplemented "Need to translate instruction to SIL.")
      end

let callees_of_function_def : LAst.function_def -> Procname.t list = function
    FunctionDef (_, _, _, annotated_instrs) ->
      let callee_of_instruction : LAst.instruction -> Procname.t option = begin function
        | Call (_, func_var, _) -> Some (procname_of_function_variable func_var)
        | _ -> None
      end in
      IList.flatten_options (
        IList.map
          (fun annotated_instr -> callee_of_instruction (fst annotated_instr))
          annotated_instrs)

(* Update CFG and call graph with new function definition *)
let trans_function_def (cfg : Cfg.cfg) (cg: Cg.t) (metadata : LAst.metadata_map)
    (func_def : LAst.function_def) : unit =

  (* each procedure has different scope: start names from id 0 *)
  Ident.NameGenerator.reset ();

  match func_def with
    FunctionDef (func_name, ret_tp_opt, params, annotated_instrs) ->
      let proc_name = procname_of_function_variable func_name in
      let ret_type =
        match ret_tp_opt with
        | None -> Sil.Tvoid
        | Some ret_tp -> trans_typ ret_tp in
      let (proc_attrs : ProcAttributes.t) =
        { (ProcAttributes.default proc_name Config.C_CPP) with
          ProcAttributes.formals =
            IList.map (fun (tp, name) -> (Mangled.from_string name, trans_typ tp)) params;
          is_defined = true; (** is defined and not just declared *)
          loc = source_only_location ();
          locals = []; (* TODO *)
          ret_type;
        } in
      let procdesc = Cfg.Procdesc.create cfg proc_attrs in
      let start_kind = Cfg.Node.Start_node procdesc in
      let start_node = Cfg.Node.create cfg (source_only_location ()) start_kind [] procdesc [] in
      let exit_kind = Cfg.Node.Exit_node procdesc in
      let exit_node = Cfg.Node.create cfg (source_only_location ()) exit_kind [] procdesc [] in
      let node_of_sil_instr cfg procdesc sil_instr =
        Cfg.Node.create cfg (Sil.instr_get_loc sil_instr) (Cfg.Node.Stmt_node "method_body")
          [sil_instr] procdesc [] in
      let rec link_nodes (start_node : Cfg.Node.t) : Cfg.Node.t list -> unit = function
        (* link all nodes in a chain for now *)
        | [] -> Cfg.Node.set_succs_exn start_node [exit_node] [exit_node]
        | nd :: nds -> Cfg.Node.set_succs_exn start_node [nd] [exit_node]; link_nodes nd nds in
      let (sil_instrs, locals) =
        trans_annotated_instructions cfg procdesc metadata annotated_instrs in
      let nodes = IList.map (node_of_sil_instr cfg procdesc) sil_instrs in
      Cfg.Procdesc.set_start_node procdesc start_node;
      Cfg.Procdesc.set_exit_node procdesc exit_node;
      link_nodes start_node nodes;
      Cfg.Node.add_locals_ret_declaration start_node locals;
      Cg.add_defined_node cg proc_name;
      IList.iter (Cg.add_edge cg proc_name) (callees_of_function_def func_def)

let trans_program : LAst.program -> Cfg.cfg * Cg.t * Tenv.t = function
    Program (func_defs, metadata) ->
      let cfg = Cfg.Node.create_cfg () in
      let cg = Cg.create () in
      let tenv = Tenv.create () in
      IList.iter (trans_function_def cfg cg metadata) func_defs;
      (cfg, cg, tenv)
