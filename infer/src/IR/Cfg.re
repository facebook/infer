/*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! IStd;

let module L = Logging;

let module F = Format;


/** data type for the control flow graph */
type cfg = {proc_desc_table: Procname.Hash.t Procdesc.t /** Map proc name to procdesc */};


/** create a new empty cfg */
let create_cfg () => {proc_desc_table: Procname.Hash.create 16};

let add_proc_desc cfg pname pdesc => Procname.Hash.add cfg.proc_desc_table pname pdesc;

let remove_proc_desc cfg pname => Procname.Hash.remove cfg.proc_desc_table pname;

let iter_proc_desc cfg f => Procname.Hash.iter f cfg.proc_desc_table;

let find_proc_desc_from_name cfg pname =>
  try (Some (Procname.Hash.find cfg.proc_desc_table pname)) {
  | Not_found => None
  };


/** Create a new procdesc */
let create_proc_desc cfg (proc_attributes: ProcAttributes.t) => {
  let pdesc = Procdesc.from_proc_attributes called_from_cfg::true proc_attributes;
  add_proc_desc cfg proc_attributes.proc_name pdesc;
  pdesc
};


/** Iterate over all the nodes in the cfg */
let iter_all_nodes sorted::sorted=false f cfg => {
  let do_proc_desc _ (pdesc: Procdesc.t) =>
    List.iter f::(fun node => f pdesc node) (Procdesc.get_nodes pdesc);
  if (not sorted) {
    iter_proc_desc cfg do_proc_desc
  } else {
    Procname.Hash.fold
      (
        fun _ pdesc desc_nodes =>
          List.fold
            f::(fun desc_nodes node => [(pdesc, node), ...desc_nodes])
            init::desc_nodes
            (Procdesc.get_nodes pdesc)
      )
      cfg.proc_desc_table
      [] |>
    List.sort cmp::[%compare : (Procdesc.t, Procdesc.Node.t)] |>
    List.iter f::(fun (d, n) => f d n)
  }
};


/** Get all the procdescs (defined and declared) */
let get_all_procs cfg => {
  let procs = ref [];
  let f _ pdesc => procs := [pdesc, ...!procs];
  iter_proc_desc cfg f;
  !procs
};


/** Get the procedures whose body is defined in this cfg */
let get_defined_procs cfg => List.filter f::Procdesc.is_defined (get_all_procs cfg);


/** checks whether a cfg is connected or not */
let check_cfg_connectedness cfg => {
  let is_exit_node n =>
    switch (Procdesc.Node.get_kind n) {
    | Procdesc.Node.Exit_node _ => true
    | _ => false
    };
  let broken_node n => {
    let succs = Procdesc.Node.get_succs n;
    let preds = Procdesc.Node.get_preds n;
    switch (Procdesc.Node.get_kind n) {
    | Procdesc.Node.Start_node _ => Int.equal (List.length succs) 0 || List.length preds > 0
    | Procdesc.Node.Exit_node _ => List.length succs > 0 || Int.equal (List.length preds) 0
    | Procdesc.Node.Stmt_node _
    | Procdesc.Node.Prune_node _
    | Procdesc.Node.Skip_node _ =>
      Int.equal (List.length succs) 0 || Int.equal (List.length preds) 0
    | Procdesc.Node.Join_node =>
      /* Join node has the exception that it may be without predecessors
         and pointing to an exit node */
      /* if the if brances end with a return */
      switch succs {
      | [n'] when is_exit_node n' => false
      | _ => Int.equal (List.length preds) 0
      }
    }
  };
  let do_pdesc pd => {
    let pname = Procname.to_string (Procdesc.get_proc_name pd);
    let nodes = Procdesc.get_nodes pd;
    let broken = List.exists f::broken_node nodes;
    if broken {
      L.out "\n ***BROKEN CFG: '%s'\n" pname
    } else {
      L.out "\n ***CONNECTED CFG: '%s'\n" pname
    }
  };
  let pdescs = get_all_procs cfg;
  List.iter f::do_pdesc pdescs
};


/** Serializer for control flow graphs */
let cfg_serializer: Serialization.serializer cfg = Serialization.create_serializer Serialization.Key.cfg;


/** Load a cfg from a file */
let load_cfg_from_file (filename: DB.filename) :option cfg =>
  Serialization.read_from_file cfg_serializer filename;


/** Save the .attr files for the procedures in the cfg. */
let save_attributes source_file cfg => {
  let save_proc pdesc => {
    let attributes = Procdesc.get_attributes pdesc;
    let loc = attributes.loc;
    let attributes' = {
      let loc' =
        if (Location.equal loc Location.dummy) {
          {...loc, file: source_file}
        } else {
          loc
        };
      {...attributes, loc: loc', source_file_captured: source_file}
    };
    AttributesTable.store_attributes attributes'
  };
  List.iter f::save_proc (get_all_procs cfg)
};


/** Inline a synthetic (access or bridge) method. */
let inline_synthetic_method ret_id etl pdesc loc_call :option Sil.instr => {
  let modified = ref None;
  let debug = false;
  let found instr instr' => {
    modified := Some instr';
    if debug {
      L.stderr "XX inline_synthetic_method found instr: %a@." (Sil.pp_instr Pp.text) instr;
      L.stderr "XX inline_synthetic_method instr': %a@." (Sil.pp_instr Pp.text) instr'
    }
  };
  let do_instr _ instr =>
    switch (instr, ret_id, etl) {
    | (
        Sil.Load _ (Exp.Lfield (Exp.Var _) fn ft) bt _,
        Some (ret_id, _),
        [(e1, _)] /* getter for fields */
      ) =>
      let instr' = Sil.Load ret_id (Exp.Lfield e1 fn ft) bt loc_call;
      found instr instr'
    | (Sil.Load _ (Exp.Lfield (Exp.Lvar pvar) fn ft) bt _, Some (ret_id, _), [])
        when Pvar.is_global pvar =>
      /* getter for static fields */
      let instr' = Sil.Load ret_id (Exp.Lfield (Exp.Lvar pvar) fn ft) bt loc_call;
      found instr instr'
    | (Sil.Store (Exp.Lfield _ fn ft) bt _ _, _, [(e1, _), (e2, _)] /* setter for fields */) =>
      let instr' = Sil.Store (Exp.Lfield e1 fn ft) bt e2 loc_call;
      found instr instr'
    | (Sil.Store (Exp.Lfield (Exp.Lvar pvar) fn ft) bt _ _, _, [(e1, _)]) when Pvar.is_global pvar =>
      /* setter for static fields */
      let instr' = Sil.Store (Exp.Lfield (Exp.Lvar pvar) fn ft) bt e1 loc_call;
      found instr instr'
    | (Sil.Call ret_id' (Exp.Const (Const.Cfun pn)) etl' _ cf, _, _)
        when
          Bool.equal (is_none ret_id) (is_none ret_id') &&
          Int.equal (List.length etl') (List.length etl) =>
      let instr' = Sil.Call ret_id (Exp.Const (Const.Cfun pn)) etl loc_call cf;
      found instr instr'
    | (Sil.Call ret_id' (Exp.Const (Const.Cfun pn)) etl' _ cf, _, _)
        when
          Bool.equal (is_none ret_id) (is_none ret_id') &&
          Int.equal (List.length etl' + 1) (List.length etl) =>
      let etl1 =
        switch (List.rev etl) {
        /* remove last element */
        | [_, ...l] => List.rev l
        | [] => assert false
        };
      let instr' = Sil.Call ret_id (Exp.Const (Const.Cfun pn)) etl1 loc_call cf;
      found instr instr'
    | _ => ()
    };
  Procdesc.iter_instrs do_instr pdesc;
  !modified
};


/** Find synthetic (access or bridge) Java methods in the procedure and inline them in the cfg. */
let proc_inline_synthetic_methods cfg pdesc :unit => {
  let instr_inline_synthetic_method =
    fun
    | Sil.Call ret_id (Exp.Const (Const.Cfun pn)) etl loc _ =>
      switch (find_proc_desc_from_name cfg pn) {
      | Some pd =>
        let is_access = Procname.java_is_access_method pn;
        let attributes = Procdesc.get_attributes pd;
        let is_synthetic = attributes.is_synthetic_method;
        let is_bridge = attributes.is_bridge_method;
        if (is_access || is_bridge || is_synthetic) {
          inline_synthetic_method ret_id etl pd loc
        } else {
          None
        }
      | None => None
      }
    | _ => None;
  let node_inline_synthetic_methods node => {
    let modified = ref false;
    let do_instr instr =>
      switch (instr_inline_synthetic_method instr) {
      | None => instr
      | Some instr' =>
        modified := true;
        instr'
      };
    let instrs = Procdesc.Node.get_instrs node;
    let instrs' = List.map f::do_instr instrs;
    if !modified {
      Procdesc.Node.replace_instrs node instrs'
    }
  };
  Procdesc.iter_nodes node_inline_synthetic_methods pdesc
};


/** Inline the java synthetic methods in the cfg */
let inline_java_synthetic_methods cfg => {
  let f pname pdesc =>
    if (Procname.is_java pname) {
      proc_inline_synthetic_methods cfg pdesc
    };
  iter_proc_desc cfg f
};


/** compute the list of procedures added or changed in [cfg_new] over [cfg_old] */
let mark_unchanged_pdescs cfg_new cfg_old => {
  let pdescs_eq (pd1: Procdesc.t) (pd2: Procdesc.t) => {
    /* map of exp names in pd1 -> exp names in pd2 */
    let exp_map = ref Exp.Map.empty;
    /* map of node id's in pd1 -> node id's in pd2 */
    let node_map = ref Procdesc.NodeMap.empty;
    /* formals are the same if their types are the same */
    let formals_eq formals1 formals2 =>
      List.equal equal::(fun (_, typ1) (_, typ2) => Typ.equal typ1 typ2) formals1 formals2;
    let nodes_eq n1s n2s => {
      /* nodes are the same if they have the same id, instructions, and succs/preds up to renaming
         with [exp_map] and [id_map] */
      let node_eq (n1: Procdesc.Node.t) (n2: Procdesc.Node.t) => {
        let compare_id (n1: Procdesc.Node.t) (n2: Procdesc.Node.t) =>
          try {
            let n1_mapping = Procdesc.NodeMap.find n1 !node_map;
            Procdesc.Node.compare n1_mapping n2
          } {
          | Not_found =>
            /* assume id's are equal and enforce by adding to [id_map] */
            node_map := Procdesc.NodeMap.add n1 n2 !node_map;
            0
          };
        let instrs_eq instrs1 instrs2 =>
          List.equal
            equal::(
              fun i1 i2 => {
                let (n, exp_map') = Sil.compare_structural_instr i1 i2 !exp_map;
                exp_map := exp_map';
                Int.equal n 0
              }
            )
            instrs1
            instrs2;
        Int.equal (compare_id n1 n2) 0 &&
        List.equal
          equal::Procdesc.Node.equal (Procdesc.Node.get_succs n1) (Procdesc.Node.get_succs n2) &&
        List.equal
          equal::Procdesc.Node.equal (Procdesc.Node.get_preds n1) (Procdesc.Node.get_preds n2) &&
        instrs_eq (Procdesc.Node.get_instrs n1) (Procdesc.Node.get_instrs n2)
      };
      try (List.for_all2_exn f::node_eq n1s n2s) {
      | Invalid_argument _ => false
      }
    };
    let att1 = Procdesc.get_attributes pd1
    and att2 = Procdesc.get_attributes pd2;
    Bool.equal att1.is_defined att2.is_defined &&
    Typ.equal att1.ret_type att2.ret_type &&
    formals_eq att1.formals att2.formals &&
    nodes_eq (Procdesc.get_nodes pd1) (Procdesc.get_nodes pd2)
  };
  let old_procs = cfg_old.proc_desc_table;
  let new_procs = cfg_new.proc_desc_table;
  let mark_pdesc_if_unchanged pname (new_pdesc: Procdesc.t) =>
    try {
      let old_pdesc = Procname.Hash.find old_procs pname;
      let changed =
        /* in continue_capture mode keep the old changed bit */
        Config.continue_capture && (Procdesc.get_attributes old_pdesc).changed ||
        not (pdescs_eq old_pdesc new_pdesc);
      (Procdesc.get_attributes new_pdesc).changed = changed
    } {
    | Not_found => ()
    };
  Procname.Hash.iter mark_pdesc_if_unchanged new_procs
};


/** Save a cfg into a file */
let store_cfg_to_file source_file::source_file (filename: DB.filename) (cfg: cfg) => {
  inline_java_synthetic_methods cfg;
  if Config.incremental_procs {
    switch (load_cfg_from_file filename) {
    | Some old_cfg => mark_unchanged_pdescs cfg old_cfg
    | None => ()
    }
  };
  /* NOTE: it's important to write attribute files to disk before writing .cfg file to disk.
     OndemandCapture module relies on it - it uses existance of .cfg file as a barrier to make
     sure that all attributes were written to disk (but not necessarily flushed) */
  save_attributes source_file cfg;
  Serialization.write_to_file cfg_serializer filename cfg
};


/** clone a procedure description and apply the type substitutions where
    the parameters are used */
let specialize_types_proc callee_pdesc resolved_pdesc substitutions => {
  let resolved_pname = Procdesc.get_proc_name resolved_pdesc
  and callee_start_node = Procdesc.get_start_node callee_pdesc
  and callee_exit_node = Procdesc.get_exit_node callee_pdesc;
  let convert_pvar pvar => Pvar.mk (Pvar.get_name pvar) resolved_pname;
  let mk_ptr_typ typename =>
    /* Only consider pointers from Java objects for now */
    Typ.Tptr (Typ.Tstruct typename) Typ.Pk_pointer;
  let convert_exp =
    fun
    | Exp.Lvar origin_pvar => Exp.Lvar (convert_pvar origin_pvar)
    | exp => exp;
  let subst_map = ref Ident.IdentMap.empty;
  let redirect_typename origin_id =>
    try (Some (Ident.IdentMap.find origin_id !subst_map)) {
    | Not_found => None
    };
  let convert_instr instrs =>
    fun
    | Sil.Load
        id (Exp.Lvar origin_pvar as origin_exp) (Typ.Tptr (Tstruct origin_typename) Pk_pointer) loc => {
        let specialized_typname =
          try (Mangled.Map.find (Pvar.get_name origin_pvar) substitutions) {
          | Not_found => origin_typename
          };
        subst_map := Ident.IdentMap.add id specialized_typname !subst_map;
        [Sil.Load id (convert_exp origin_exp) (mk_ptr_typ specialized_typname) loc, ...instrs]
      }
    | Sil.Load id (Exp.Var origin_id as origin_exp) (Typ.Tstruct _ as origin_typ) loc => {
        let updated_typ =
          try (Typ.Tstruct (Ident.IdentMap.find origin_id !subst_map)) {
          | Not_found => origin_typ
          };
        [Sil.Load id (convert_exp origin_exp) updated_typ loc, ...instrs]
      }
    | Sil.Load id origin_exp origin_typ loc => [
        Sil.Load id (convert_exp origin_exp) origin_typ loc,
        ...instrs
      ]
    | Sil.Store assignee_exp origin_typ origin_exp loc => {
        let set_instr =
          Sil.Store (convert_exp assignee_exp) origin_typ (convert_exp origin_exp) loc;
        [set_instr, ...instrs]
      }
    | Sil.Call
        return_ids
        (Exp.Const (Const.Cfun (Procname.Java callee_pname_java)))
        [(Exp.Var id, _), ...origin_args]
        loc
        call_flags
        when call_flags.CallFlags.cf_virtual && redirect_typename id != None => {
        let redirected_typename = Option.value_exn (redirect_typename id);
        let redirected_typ = mk_ptr_typ redirected_typename;
        let redirected_pname =
          Procname.replace_class
            (Procname.Java callee_pname_java) (Typename.name redirected_typename);
        let args = {
          let other_args = List.map f::(fun (exp, typ) => (convert_exp exp, typ)) origin_args;
          [(Exp.Var id, redirected_typ), ...other_args]
        };
        let call_instr =
          Sil.Call return_ids (Exp.Const (Const.Cfun redirected_pname)) args loc call_flags;
        [call_instr, ...instrs]
      }
    | Sil.Call return_ids origin_call_exp origin_args loc call_flags => {
        let converted_args = List.map f::(fun (exp, typ) => (convert_exp exp, typ)) origin_args;
        let call_instr =
          Sil.Call return_ids (convert_exp origin_call_exp) converted_args loc call_flags;
        [call_instr, ...instrs]
      }
    | Sil.Prune origin_exp loc is_true_branch if_kind => [
        Sil.Prune (convert_exp origin_exp) loc is_true_branch if_kind,
        ...instrs
      ]
    | Sil.Declare_locals typed_vars loc => {
        let new_typed_vars = List.map f::(fun (pvar, typ) => (convert_pvar pvar, typ)) typed_vars;
        [Sil.Declare_locals new_typed_vars loc, ...instrs]
      }
    | Sil.Nullify _
    | Abstract _
    | Sil.Remove_temps _ =>
      /* these are generated instructions that will be replaced by the preanalysis */
      instrs;
  let convert_node_kind =
    fun
    | Procdesc.Node.Start_node _ => Procdesc.Node.Start_node resolved_pname
    | Procdesc.Node.Exit_node _ => Procdesc.Node.Exit_node resolved_pname
    | node_kind => node_kind;
  let node_map = ref Procdesc.NodeMap.empty;
  let rec convert_node node => {
    let loc = Procdesc.Node.get_loc node
    and kind = convert_node_kind (Procdesc.Node.get_kind node)
    and instrs = List.fold f::convert_instr init::[] (Procdesc.Node.get_instrs node) |> List.rev;
    Procdesc.create_node resolved_pdesc loc kind instrs
  }
  and loop callee_nodes =>
    switch callee_nodes {
    | [] => []
    | [node, ...other_node] =>
      let converted_node =
        try (Procdesc.NodeMap.find node !node_map) {
        | Not_found =>
          let new_node = convert_node node
          and successors = Procdesc.Node.get_succs node
          and exn_nodes = Procdesc.Node.get_exn node;
          node_map := Procdesc.NodeMap.add node new_node !node_map;
          if (Procdesc.Node.equal node callee_start_node) {
            Procdesc.set_start_node resolved_pdesc new_node
          };
          if (Procdesc.Node.equal node callee_exit_node) {
            Procdesc.set_exit_node resolved_pdesc new_node
          };
          Procdesc.node_set_succs_exn callee_pdesc new_node (loop successors) (loop exn_nodes);
          new_node
        };
      [converted_node, ...loop other_node]
    };
  ignore (loop [callee_start_node]);
  resolved_pdesc
};


/** Creates a copy of a procedure description and a list of type substitutions of the form
    (name, typ) where name is a parameter. The resulting proc desc is isomorphic but
    all the type of the parameters are replaced in the instructions according to the list.
    The virtual calls are also replaced to match the parameter types */
let specialize_types callee_pdesc resolved_pname args => {
  let callee_attributes = Procdesc.get_attributes callee_pdesc;
  let (resolved_params, substitutions) =
    List.fold2_exn
      f::(
        fun (params, subts) (param_name, param_typ) (_, arg_typ) =>
          switch arg_typ {
          | Typ.Tptr (Tstruct typename) Pk_pointer =>
            /* Replace the type of the parameter by the type of the argument */
            ([(param_name, arg_typ), ...params], Mangled.Map.add param_name typename subts)
          | _ => ([(param_name, param_typ), ...params], subts)
          }
      )
      init::([], Mangled.Map.empty)
      callee_attributes.formals
      args;
  let resolved_attributes = {
    ...callee_attributes,
    formals: List.rev resolved_params,
    proc_name: resolved_pname
  };
  AttributesTable.store_attributes resolved_attributes;
  let resolved_pdesc = {
    let tmp_cfg = create_cfg ();
    create_proc_desc tmp_cfg resolved_attributes
  };
  specialize_types_proc callee_pdesc resolved_pdesc substitutions
};

let pp_proc_signatures fmt cfg => {
  F.fprintf fmt "METHOD SIGNATURES\n@.";
  let sorted_procs = List.sort cmp::Procdesc.compare (get_all_procs cfg);
  List.iter f::(fun pdesc => F.fprintf fmt "%a@." Procdesc.pp_signature pdesc) sorted_procs
};
