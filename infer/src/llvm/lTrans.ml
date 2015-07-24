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
exception Unimplemented of string

let gen_variable : variable -> Sil.exp = function (* HACK *)
  | Global id -> Sil.Var (Ident.create_normal (Ident.string_to_name id) 0)
  | Local id -> Sil.Var (Ident.create_normal (Ident.string_to_name id) 0)

let gen_constant : constant -> Sil.exp = function
  | Cint i -> Sil.Const (Sil.Cint (Sil.Int.of_int i))
  | Cnull -> Sil.exp_null

let gen_operand : operand -> Sil.exp = function
  | Var var -> gen_variable var
  | Const const -> gen_constant const

let rec gen_typ : typ -> Sil.typ = function
  | Tint i -> Sil.Tint IInt (* need to actually check what size int is needed here *)
  | Tfloat -> Sil.Tfloat FFloat
  | Tptr tp -> Tptr (gen_typ tp, Pk_pointer)
  | Tvector (i, tp)
  | Tarray (i, tp) -> Sil.Tarray (gen_typ tp, Sil.Const (Sil.Cint (Sil.Int.of_int i)))
  | Tlabel -> raise (ImproperTypeError "Tried to generate Sil type from LLVM label type.")
  | Tmetadata -> raise (ImproperTypeError "Tried to generate Sil type from LLVM metadata type.")

let gen_instr (cfg : Cfg.cfg) (pdesc : Cfg.Procdesc.t) : instr -> Sil.instr list = function
  | Ret None -> []
  | Ret (Some (tp, exp)) ->
      let ret_var = Sil.get_ret_pvar (Cfg.Procdesc.get_proc_name pdesc) in
      [Sil.Set (Sil.Lvar ret_var, gen_typ tp, gen_operand exp, Sil.dummy_location)]
  | _ -> raise (Unimplemented "Need to translate instruction to SIL.")

(* Modify the cfg in place *)
let gen_func_def (old_cfg : Cfg.cfg) : func_def -> unit = function
  FuncDef (func_name, ret_tp_opt, params, instrs) ->
    let (proc_attrs : Sil.proc_attributes) =
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
    let (pdesc_builder : Cfg.Procdesc.proc_desc_builder) =
      { cfg = old_cfg;
        name = Procname.from_string_c_fun (string_of_variable func_name);
        is_defined = true; (** is defined and not just declared *)
        proc_attributes = proc_attrs;
        ret_type = (match ret_tp_opt with
            | None -> Sil.Tvoid
            | Some ret_tp -> gen_typ ret_tp);
        formals = List.map (fun (tp, name) -> (name, gen_typ tp)) params;
        locals = []; (* TODO *)
        captured = [];
        loc = Sil.dummy_location
      } in
    let nodekind_of_instr : instr -> Cfg.Node.nodekind = function
      | Ret _ -> Cfg.Node.Stmt_node "method_body"
      | _ -> raise (Unimplemented "Need to get node type for instruction.") in
    let add_instr (cfg : Cfg.cfg) (pdesc : Cfg.Procdesc.t) (ins : instr) : unit =
      Cfg.Node.create cfg Sil.dummy_location (nodekind_of_instr ins)
          (gen_instr cfg pdesc ins) pdesc []; () in
    let pdesc = Cfg.Procdesc.create pdesc_builder in
    List.iter (fun ins -> add_instr old_cfg pdesc ins) instrs

let gen_prog : prog -> Cfg.cfg = function
  Prog fds ->
    let cfg = Cfg.Node.create_cfg () in
    List.iter (gen_func_def cfg) fds; cfg
