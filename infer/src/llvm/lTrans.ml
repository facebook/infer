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
  | Tint i -> Sil.Tint Sil.IInt (* TODO: check what size int is needed here *)
  | Tfloat -> Sil.Tfloat Sil.FFloat
  | Tptr tp -> Sil.Tptr (trans_typ tp, Sil.Pk_pointer)
  | Tvector (i, tp)
  | Tarray (i, tp) -> Sil.Tarray (trans_typ tp, Sil.Const (Sil.Cint (Sil.Int.of_int i)))
  | Tfunc _ -> Sil.Tfun false
  | Tlabel -> raise (ImproperTypeError "Tried to generate Sil type from LLVM label type.")
  | Tmetadata -> raise (ImproperTypeError "Tried to generate Sil type from LLVM metadata type.")

let get_source_filename (metadata : LAst.metadata_map) : DB.source_file =
  match MetadataMap.find 1 metadata with
  | MetadataVal (MetadataString file_str) :: _ -> DB.source_file_from_string file_str
  | _ -> raise (MalformedMetadata "Source file name was expected at head of metadata variable !1.")

let location_of_annotation_option (metadata : LAst.metadata_map)
  : LAst.annotation option -> Sil.location = function
  | None -> Sil.dummy_location (* no annotation means no source location *)
  | Some (Annotation i) ->
      begin match MetadataMap.find i metadata with
        | TypOperand (_, Const (Cint line_num)) :: _ -> let open Sil in
            { line = line_num; col = -1; file = get_source_filename metadata; nLOC = -1 }
        | [] -> raise (MalformedMetadata "Instruction annotation refers to empty metadata node.")
        | _ -> raise (MalformedMetadata ("Instruction annotation refers to metadata node " ^
                                         "without line number as first component."))
      end

(* Generate list of SIL instructions and list of local variables *)
let rec trans_annotated_instrs
    (cfg : Cfg.cfg) (procdesc : Cfg.Procdesc.t) (metadata : LAst.metadata_map)
  : LAst.annotated_instr list -> Sil.instr list * (Mangled.t * Sil.typ) list = function
  | [] -> ([], [])
  | (instr, anno) :: t ->
      let (sil_instrs, locals) = trans_annotated_instrs cfg procdesc metadata t in
      let location = location_of_annotation_option metadata anno in
      begin match instr with
        | Ret None -> (sil_instrs, locals)
        | Ret (Some (tp, exp)) ->
            let procname = Cfg.Procdesc.get_proc_name procdesc in
            let ret_var = Sil.get_ret_pvar procname in
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
      | Alloc (var, tp, num_elems) ->
          (* num_elems currently ignored *)
          begin match var with
          | Global (Name var_name) | Local (Name var_name) ->
              let new_local = (Mangled.from_string var_name, trans_typ (Tptr tp)) in
              (sil_instrs, new_local :: locals)
          | _ -> raise (ImproperTypeError "Not expecting alloca instruction to a numbered variable.")
          end
      | _ -> raise (Unimplemented "Need to translate instruction to SIL.")
      end

(* Update CFG and call graph with new function definition *)
let trans_func_def (cfg : Cfg.cfg) (cg: Cg.t) (metadata : LAst.metadata_map)
  : LAst.func_def -> unit = function
    FuncDef (func_name, ret_tp_opt, params, annotated_instrs) ->
      let (proc_attrs : Sil.proc_attributes) =
        let open Sil in
        { access = Sil.Default;
          exceptions = [];
          is_abstract = false;
          is_bridge_method = false;
          is_objc_instance_method = false;
          is_synthetic_method = false;
          language = Sil.C_CPP;
          func_attributes = [];
          method_annotation = Sil.method_annotation_empty;
          is_generated = false
        } in
      let (procdesc_builder : Cfg.Procdesc.proc_desc_builder) =
        let open Cfg.Procdesc in
        { cfg = cfg;
          name = Procname.from_string_c_fun (LAst.string_of_variable func_name);
          is_defined = true; (** is defined and not just declared *)
          proc_attributes = proc_attrs;
          ret_type = (match ret_tp_opt with
              | None -> Sil.Tvoid
              | Some ret_tp -> trans_typ ret_tp);
          formals = Utils.list_map (fun (tp, name) -> (name, trans_typ tp)) params;
          locals = []; (* TODO *)
          captured = [];
          loc = Sil.dummy_location
        } in
      let procdesc = Cfg.Procdesc.create procdesc_builder in
      let procname = Cfg.Procdesc.get_proc_name procdesc in
      let start_kind = Cfg.Node.Start_node procdesc in
      let start_node = Cfg.Node.create cfg Sil.dummy_location start_kind [] procdesc [] in
      let exit_kind = Cfg.Node.Exit_node procdesc in
      let exit_node = Cfg.Node.create cfg Sil.dummy_location exit_kind [] procdesc [] in
      let node_of_sil_instr cfg procdesc sil_instr =
        Cfg.Node.create cfg Sil.dummy_location (Cfg.Node.Stmt_node "method_body")
            [sil_instr] procdesc [] in
      let rec link_nodes (start_node : Cfg.Node.t) : Cfg.Node.t list -> unit = function
        (* link all nodes in a chain for now *)
        | [] -> Cfg.Node.set_succs_exn start_node [exit_node] [exit_node]
        | nd :: nds -> Cfg.Node.set_succs_exn start_node [nd] [exit_node]; link_nodes nd nds in
      let (sil_instrs, locals) = trans_annotated_instrs cfg procdesc metadata annotated_instrs in
      let nodes = Utils.list_map (node_of_sil_instr cfg procdesc) sil_instrs in
      Cfg.Procdesc.set_start_node procdesc start_node;
      Cfg.Procdesc.set_exit_node procdesc exit_node;
      link_nodes start_node nodes;
      Cfg.Node.add_locals_ret_declaration start_node locals;
      Cg.add_node cg procname

let trans_prog : LAst.prog -> Cfg.cfg * Cg.t * Sil.tenv = function
    Prog (func_defs, metadata) ->
      let cfg = Cfg.Node.create_cfg () in
      let cg = Cg.create () in
      let tenv = Sil.create_tenv () in
      Utils.list_iter (trans_func_def cfg cg metadata) func_defs;
      (cfg, cg, tenv)
