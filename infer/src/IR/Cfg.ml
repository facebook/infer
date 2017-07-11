(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd
module L = Logging
module F = Format

(** data type for the control flow graph *)
type cfg = {proc_desc_table: (** Map proc name to procdesc *) Procdesc.t Typ.Procname.Hash.t}

(** create a new empty cfg *)
let create_cfg () = {proc_desc_table= Typ.Procname.Hash.create 16}

let add_proc_desc cfg pname pdesc = Typ.Procname.Hash.add cfg.proc_desc_table pname pdesc

let remove_proc_desc cfg pname = Typ.Procname.Hash.remove cfg.proc_desc_table pname

let iter_proc_desc cfg f = Typ.Procname.Hash.iter f cfg.proc_desc_table

let find_proc_desc_from_name cfg pname =
  try Some (Typ.Procname.Hash.find cfg.proc_desc_table pname)
  with Not_found -> None

(** Create a new procdesc *)
let create_proc_desc cfg (proc_attributes: ProcAttributes.t) =
  let pdesc = Procdesc.from_proc_attributes ~called_from_cfg:true proc_attributes in
  add_proc_desc cfg proc_attributes.proc_name pdesc ; pdesc

(** Iterate over all the nodes in the cfg *)
let iter_all_nodes ?(sorted= false) f cfg =
  let do_proc_desc _ (pdesc: Procdesc.t) =
    List.iter ~f:(fun node -> f pdesc node) (Procdesc.get_nodes pdesc)
  in
  if not sorted then iter_proc_desc cfg do_proc_desc
  else
    Typ.Procname.Hash.fold
      (fun _ pdesc desc_nodes ->
        List.fold
          ~f:(fun desc_nodes node -> (pdesc, node) :: desc_nodes)
          ~init:desc_nodes (Procdesc.get_nodes pdesc))
      cfg.proc_desc_table []
    |> List.sort ~cmp:[%compare : Procdesc.t * Procdesc.Node.t]
    |> List.iter ~f:(fun (d, n) -> f d n)

(** Get all the procdescs (defined and declared) *)
let get_all_procs cfg =
  let procs = ref [] in
  let f _ pdesc = procs := pdesc :: !procs in
  iter_proc_desc cfg f ; !procs

(** Get the procedures whose body is defined in this cfg *)
let get_defined_procs cfg = List.filter ~f:Procdesc.is_defined (get_all_procs cfg)

(** checks whether a cfg is connected or not *)
let check_cfg_connectedness cfg =
  let is_exit_node n =
    match Procdesc.Node.get_kind n with Procdesc.Node.Exit_node _ -> true | _ -> false
  in
  let broken_node n =
    let succs = Procdesc.Node.get_succs n in
    let preds = Procdesc.Node.get_preds n in
    match Procdesc.Node.get_kind n with
    | Procdesc.Node.Start_node _
     -> Int.equal (List.length succs) 0 || List.length preds > 0
    | Procdesc.Node.Exit_node _
     -> List.length succs > 0 || Int.equal (List.length preds) 0
    | Procdesc.Node.Stmt_node _ | Procdesc.Node.Prune_node _ | Procdesc.Node.Skip_node _
     -> Int.equal (List.length succs) 0 || Int.equal (List.length preds) 0
    | Procdesc.Node.Join_node ->
      (* Join node has the exception that it may be without predecessors
         and pointing to an exit node *)
      (* if the if brances end with a return *)
      match succs with [n'] when is_exit_node n' -> false | _ -> Int.equal (List.length preds) 0
  in
  let do_pdesc pd =
    let pname = Typ.Procname.to_string (Procdesc.get_proc_name pd) in
    let nodes = Procdesc.get_nodes pd in
    let broken = List.exists ~f:broken_node nodes in
    if broken then L.internal_error "@\n ***BROKEN CFG: '%s'@\n" pname
  in
  let pdescs = get_all_procs cfg in
  List.iter ~f:do_pdesc pdescs

(** Serializer for control flow graphs *)
let cfg_serializer : cfg Serialization.serializer =
  Serialization.create_serializer Serialization.Key.cfg

(** Load a cfg from a file *)
let load_cfg_from_file (filename: DB.filename) : cfg option =
  Serialization.read_from_file cfg_serializer filename

(** Save the .attr files for the procedures in the cfg. *)
let save_attributes source_file cfg =
  let save_proc pdesc =
    let attributes = Procdesc.get_attributes pdesc in
    let loc = attributes.loc in
    let attributes' =
      let loc' = if Location.equal loc Location.dummy then {loc with file= source_file} else loc in
      {attributes with loc= loc'; source_file_captured= source_file}
    in
    AttributesTable.store_attributes attributes'
  in
  List.iter ~f:save_proc (get_all_procs cfg)

(** Inline a synthetic (access or bridge) method. *)
let inline_synthetic_method ret_id etl pdesc loc_call : Sil.instr option =
  let modified = ref None in
  let found instr instr' =
    modified := Some instr' ;
    L.(debug Analysis Verbose)
      "XX inline_synthetic_method found instr: %a@." (Sil.pp_instr Pp.text) instr ;
    L.(debug Analysis Verbose)
      "XX inline_synthetic_method instr': %a@." (Sil.pp_instr Pp.text) instr'
  in
  let do_instr _ instr =
    match (instr, ret_id, etl) with
    | ( Sil.Load (_, Exp.Lfield (Exp.Var _, fn, ft), bt, _)
      , Some (ret_id, _)
      , [(* getter for fields *) (e1, _)] )
     -> let instr' = Sil.Load (ret_id, Exp.Lfield (e1, fn, ft), bt, loc_call) in
        found instr instr'
    | Sil.Load (_, Exp.Lfield (Exp.Lvar pvar, fn, ft), bt, _), Some (ret_id, _), []
      when Pvar.is_global pvar
     -> (* getter for static fields *)
        let instr' = Sil.Load (ret_id, Exp.Lfield (Exp.Lvar pvar, fn, ft), bt, loc_call) in
        found instr instr'
    | Sil.Store (Exp.Lfield (_, fn, ft), bt, _, _), _, [(* setter for fields *) (e1, _); (e2, _)]
     -> let instr' = Sil.Store (Exp.Lfield (e1, fn, ft), bt, e2, loc_call) in
        found instr instr'
    | Sil.Store (Exp.Lfield (Exp.Lvar pvar, fn, ft), bt, _, _), _, [(e1, _)]
      when Pvar.is_global pvar
     -> (* setter for static fields *)
        let instr' = Sil.Store (Exp.Lfield (Exp.Lvar pvar, fn, ft), bt, e1, loc_call) in
        found instr instr'
    | Sil.Call (ret_id', Exp.Const Const.Cfun pn, etl', _, cf), _, _
      when Bool.equal (is_none ret_id) (is_none ret_id')
           && Int.equal (List.length etl') (List.length etl)
     -> let instr' = Sil.Call (ret_id, Exp.Const (Const.Cfun pn), etl, loc_call, cf) in
        found instr instr'
    | Sil.Call (ret_id', Exp.Const Const.Cfun pn, etl', _, cf), _, _
      when Bool.equal (is_none ret_id) (is_none ret_id')
           && Int.equal (List.length etl' + 1) (List.length etl)
     -> let etl1 =
          match List.rev etl with
          (* remove last element *)
          | _ :: l
           -> List.rev l
          | []
           -> assert false
        in
        let instr' = Sil.Call (ret_id, Exp.Const (Const.Cfun pn), etl1, loc_call, cf) in
        found instr instr'
    | _
     -> ()
  in
  Procdesc.iter_instrs do_instr pdesc ; !modified

(** Find synthetic (access or bridge) Java methods in the procedure and inline them in the cfg. *)
let proc_inline_synthetic_methods cfg pdesc : unit =
  let instr_inline_synthetic_method = function
    | Sil.Call (ret_id, Exp.Const Const.Cfun pn, etl, loc, _) -> (
      match find_proc_desc_from_name cfg pn with
      | Some pd
       -> let is_access = Typ.Procname.java_is_access_method pn in
          let attributes = Procdesc.get_attributes pd in
          let is_synthetic = attributes.is_synthetic_method in
          let is_bridge = attributes.is_bridge_method in
          if is_access || is_bridge || is_synthetic then inline_synthetic_method ret_id etl pd loc
          else None
      | None
       -> None )
    | _
     -> None
  in
  let node_inline_synthetic_methods node =
    let modified = ref false in
    let do_instr instr =
      match instr_inline_synthetic_method instr with
      | None
       -> instr
      | Some instr'
       -> modified := true ;
          instr'
    in
    let instrs = Procdesc.Node.get_instrs node in
    let instrs' = List.map ~f:do_instr instrs in
    if !modified then Procdesc.Node.replace_instrs node instrs'
  in
  Procdesc.iter_nodes node_inline_synthetic_methods pdesc

(** Inline the java synthetic methods in the cfg *)
let inline_java_synthetic_methods cfg =
  let f pname pdesc = if Typ.Procname.is_java pname then proc_inline_synthetic_methods cfg pdesc in
  iter_proc_desc cfg f

(** compute the list of procedures added or changed in [cfg_new] over [cfg_old] *)
let mark_unchanged_pdescs cfg_new cfg_old =
  let pdescs_eq (pd1: Procdesc.t) (pd2: Procdesc.t) =
    (* map of exp names in pd1 -> exp names in pd2 *)
    let exp_map = ref Exp.Map.empty in
    (* map of node id's in pd1 -> node id's in pd2 *)
    let node_map = ref Procdesc.NodeMap.empty in
    (* formals are the same if their types are the same *)
    let formals_eq formals1 formals2 =
      List.equal ~equal:(fun (_, typ1) (_, typ2) -> Typ.equal typ1 typ2) formals1 formals2
    in
    let nodes_eq n1s n2s =
      (* nodes are the same if they have the same id, instructions, and succs/preds up to renaming
         with [exp_map] and [id_map] *)
      let node_eq (n1: Procdesc.Node.t) (n2: Procdesc.Node.t) =
        let compare_id (n1: Procdesc.Node.t) (n2: Procdesc.Node.t) =
          try
            let n1_mapping = Procdesc.NodeMap.find n1 !node_map in
            Procdesc.Node.compare n1_mapping n2
          with Not_found ->
            (* assume id's are equal and enforce by adding to [id_map] *)
            node_map := Procdesc.NodeMap.add n1 n2 !node_map ;
            0
        in
        let instrs_eq instrs1 instrs2 =
          List.equal
            ~equal:(fun i1 i2 ->
              let n, exp_map' = Sil.compare_structural_instr i1 i2 !exp_map in
              exp_map := exp_map' ;
              Int.equal n 0)
            instrs1 instrs2
        in
        Int.equal (compare_id n1 n2) 0
        && List.equal ~equal:Procdesc.Node.equal (Procdesc.Node.get_succs n1)
             (Procdesc.Node.get_succs n2)
        && List.equal ~equal:Procdesc.Node.equal (Procdesc.Node.get_preds n1)
             (Procdesc.Node.get_preds n2)
        && instrs_eq (Procdesc.Node.get_instrs n1) (Procdesc.Node.get_instrs n2)
      in
      try List.for_all2_exn ~f:node_eq n1s n2s
      with Invalid_argument _ -> false
    in
    let att1 = Procdesc.get_attributes pd1 and att2 = Procdesc.get_attributes pd2 in
    Bool.equal att1.is_defined att2.is_defined && Typ.equal att1.ret_type att2.ret_type
    && formals_eq att1.formals att2.formals
    && nodes_eq (Procdesc.get_nodes pd1) (Procdesc.get_nodes pd2)
  in
  let old_procs = cfg_old.proc_desc_table in
  let new_procs = cfg_new.proc_desc_table in
  let mark_pdesc_if_unchanged pname (new_pdesc: Procdesc.t) =
    try
      let old_pdesc = Typ.Procname.Hash.find old_procs pname in
      let changed =
        (* in continue_capture mode keep the old changed bit *)
        Config.continue_capture && (Procdesc.get_attributes old_pdesc).changed
        || not (pdescs_eq old_pdesc new_pdesc)
      in
      (Procdesc.get_attributes new_pdesc).changed <- changed
    with Not_found -> ()
  in
  Typ.Procname.Hash.iter mark_pdesc_if_unchanged new_procs

(** Save a cfg into a file *)
let store_cfg_to_file ~source_file (filename: DB.filename) (cfg: cfg) =
  inline_java_synthetic_methods cfg ;
  ( if Config.incremental_procs then
      match load_cfg_from_file filename with
      | Some old_cfg
       -> mark_unchanged_pdescs cfg old_cfg
      | None
       -> () ) ;
  (* NOTE: it's important to write attribute files to disk before writing .cfg file to disk.
     OndemandCapture module relies on it - it uses existance of .cfg file as a barrier to make
     sure that all attributes were written to disk (but not necessarily flushed) *)
  save_attributes source_file cfg ; Serialization.write_to_file cfg_serializer filename ~data:cfg

(** clone a procedure description and apply the type substitutions where
    the parameters are used *)
let specialize_types_proc callee_pdesc resolved_pdesc substitutions =
  let resolved_pname = Procdesc.get_proc_name resolved_pdesc
  and callee_start_node = Procdesc.get_start_node callee_pdesc
  and callee_exit_node = Procdesc.get_exit_node callee_pdesc in
  let convert_pvar pvar = Pvar.mk (Pvar.get_name pvar) resolved_pname in
  let mk_ptr_typ typename =
    (* Only consider pointers from Java objects for now *)
    Typ.mk (Tptr (Typ.mk (Tstruct typename), Typ.Pk_pointer))
  in
  let convert_exp = function
    | Exp.Lvar origin_pvar
     -> Exp.Lvar (convert_pvar origin_pvar)
    | exp
     -> exp
  in
  let subst_map = ref Ident.IdentMap.empty in
  let redirect_typename origin_id =
    try Some (Ident.IdentMap.find origin_id !subst_map)
    with Not_found -> None
  in
  let convert_instr instrs = function
    | Sil.Load
        ( id
        , (Exp.Lvar origin_pvar as origin_exp)
        , {Typ.desc= Tptr ({desc= Tstruct origin_typename}, Pk_pointer)}
        , loc )
     -> let specialized_typname =
          try Mangled.Map.find (Pvar.get_name origin_pvar) substitutions
          with Not_found -> origin_typename
        in
        subst_map := Ident.IdentMap.add id specialized_typname !subst_map ;
        Sil.Load (id, convert_exp origin_exp, mk_ptr_typ specialized_typname, loc) :: instrs
    | Sil.Load (id, (Exp.Var origin_id as origin_exp), ({Typ.desc= Tstruct _} as origin_typ), loc)
     -> let updated_typ : Typ.t =
          try Typ.mk ~default:origin_typ (Tstruct (Ident.IdentMap.find origin_id !subst_map))
          with Not_found -> origin_typ
        in
        Sil.Load (id, convert_exp origin_exp, updated_typ, loc) :: instrs
    | Sil.Load (id, origin_exp, origin_typ, loc)
     -> Sil.Load (id, convert_exp origin_exp, origin_typ, loc) :: instrs
    | Sil.Store (assignee_exp, origin_typ, origin_exp, loc)
     -> let set_instr =
          Sil.Store (convert_exp assignee_exp, origin_typ, convert_exp origin_exp, loc)
        in
        set_instr :: instrs
    | Sil.Call
        ( return_ids
        , Exp.Const Const.Cfun Typ.Procname.Java callee_pname_java
        , (Exp.Var id, _) :: origin_args
        , loc
        , call_flags )
      when call_flags.CallFlags.cf_virtual && redirect_typename id <> None
     -> let redirected_typename = Option.value_exn (redirect_typename id) in
        let redirected_typ = mk_ptr_typ redirected_typename in
        let redirected_pname =
          Typ.Procname.replace_class (Typ.Procname.Java callee_pname_java) redirected_typename
        in
        let args =
          let other_args = List.map ~f:(fun (exp, typ) -> (convert_exp exp, typ)) origin_args in
          (Exp.Var id, redirected_typ) :: other_args
        in
        let call_instr =
          Sil.Call (return_ids, Exp.Const (Const.Cfun redirected_pname), args, loc, call_flags)
        in
        call_instr :: instrs
    | Sil.Call (return_ids, origin_call_exp, origin_args, loc, call_flags)
     -> let converted_args = List.map ~f:(fun (exp, typ) -> (convert_exp exp, typ)) origin_args in
        let call_instr =
          Sil.Call (return_ids, convert_exp origin_call_exp, converted_args, loc, call_flags)
        in
        call_instr :: instrs
    | Sil.Prune (origin_exp, loc, is_true_branch, if_kind)
     -> Sil.Prune (convert_exp origin_exp, loc, is_true_branch, if_kind) :: instrs
    | Sil.Declare_locals (typed_vars, loc)
     -> let new_typed_vars =
          List.map ~f:(fun (pvar, typ) -> (convert_pvar pvar, typ)) typed_vars
        in
        Sil.Declare_locals (new_typed_vars, loc) :: instrs
    | Sil.Nullify _ | Abstract _ | Sil.Remove_temps _
     -> (* these are generated instructions that will be replaced by the preanalysis *)
        instrs
  in
  let convert_node_kind = function
    | Procdesc.Node.Start_node _
     -> Procdesc.Node.Start_node resolved_pname
    | Procdesc.Node.Exit_node _
     -> Procdesc.Node.Exit_node resolved_pname
    | node_kind
     -> node_kind
  in
  let node_map = ref Procdesc.NodeMap.empty in
  let rec convert_node node =
    let loc = Procdesc.Node.get_loc node
    and kind = convert_node_kind (Procdesc.Node.get_kind node)
    and instrs = List.fold ~f:convert_instr ~init:[] (Procdesc.Node.get_instrs node) |> List.rev in
    Procdesc.create_node resolved_pdesc loc kind instrs
  and loop callee_nodes =
    match callee_nodes with
    | []
     -> []
    | node :: other_node
     -> let converted_node =
          try Procdesc.NodeMap.find node !node_map
          with Not_found ->
            let new_node = convert_node node
            and successors = Procdesc.Node.get_succs node
            and exn_nodes = Procdesc.Node.get_exn node in
            node_map := Procdesc.NodeMap.add node new_node !node_map ;
            if Procdesc.Node.equal node callee_start_node then
              Procdesc.set_start_node resolved_pdesc new_node ;
            if Procdesc.Node.equal node callee_exit_node then
              Procdesc.set_exit_node resolved_pdesc new_node ;
            Procdesc.node_set_succs_exn callee_pdesc new_node (loop successors) (loop exn_nodes) ;
            new_node
        in
        converted_node :: loop other_node
  in
  ignore (loop [callee_start_node]) ;
  resolved_pdesc

(** Creates a copy of a procedure description and a list of type substitutions of the form
    (name, typ) where name is a parameter. The resulting proc desc is isomorphic but
    all the type of the parameters are replaced in the instructions according to the list.
    The virtual calls are also replaced to match the parameter types *)
let specialize_types callee_pdesc resolved_pname args =
  let callee_attributes = Procdesc.get_attributes callee_pdesc in
  let resolved_params, substitutions =
    List.fold2_exn
      ~f:(fun (params, subts) (param_name, param_typ) (_, arg_typ) ->
        match arg_typ.Typ.desc with
        | Tptr ({desc= Tstruct typename}, Pk_pointer)
         -> (* Replace the type of the parameter by the type of the argument *)
            ((param_name, arg_typ) :: params, Mangled.Map.add param_name typename subts)
        | _
         -> ((param_name, param_typ) :: params, subts))
      ~init:([], Mangled.Map.empty) callee_attributes.formals args
  in
  let resolved_attributes =
    {callee_attributes with formals= List.rev resolved_params; proc_name= resolved_pname}
  in
  AttributesTable.store_attributes resolved_attributes ;
  let resolved_pdesc =
    let tmp_cfg = create_cfg () in
    create_proc_desc tmp_cfg resolved_attributes
  in
  specialize_types_proc callee_pdesc resolved_pdesc substitutions

let pp_proc_signatures fmt cfg =
  F.fprintf fmt "METHOD SIGNATURES@\n@." ;
  let sorted_procs = List.sort ~cmp:Procdesc.compare (get_all_procs cfg) in
  List.iter ~f:(fun pdesc -> F.fprintf fmt "%a@." Procdesc.pp_signature pdesc) sorted_procs
