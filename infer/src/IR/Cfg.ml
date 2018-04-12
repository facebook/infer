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
type t = Procdesc.t Typ.Procname.Hash.t

let create () = Typ.Procname.Hash.create 16

let get_all_proc_descs cfg =
  let procs = ref [] in
  let f _ pdesc = procs := pdesc :: !procs in
  Typ.Procname.Hash.iter f cfg ; !procs


let get_all_proc_names cfg =
  let procs = ref [] in
  let f pname _ = procs := pname :: !procs in
  Typ.Procname.Hash.iter f cfg ; !procs


(** Create a new procdesc *)
let create_proc_desc cfg (proc_attributes: ProcAttributes.t) =
  let pdesc = Procdesc.from_proc_attributes proc_attributes in
  Typ.Procname.Hash.add cfg proc_attributes.proc_name pdesc ;
  pdesc


(** Iterate over all the nodes in the cfg *)
let iter_all_nodes ?(sorted= false) f cfg =
  let do_proc_desc _ (pdesc: Procdesc.t) =
    List.iter ~f:(fun node -> f pdesc node) (Procdesc.get_nodes pdesc)
  in
  if not sorted then Typ.Procname.Hash.iter do_proc_desc cfg
  else
    Typ.Procname.Hash.fold
      (fun _ pdesc desc_nodes ->
        List.fold
          ~f:(fun desc_nodes node -> (pdesc, node) :: desc_nodes)
          ~init:desc_nodes (Procdesc.get_nodes pdesc) )
      cfg []
    |> List.sort ~cmp:[%compare : Procdesc.t * Procdesc.Node.t]
    |> List.iter ~f:(fun (d, n) -> f d n)


let is_proc_cfg_connected proc_desc =
  let is_exit_node n =
    match Procdesc.Node.get_kind n with Procdesc.Node.Exit_node _ -> true | _ -> false
  in
  let is_broken_node n =
    let succs = Procdesc.Node.get_succs n in
    let preds = Procdesc.Node.get_preds n in
    match Procdesc.Node.get_kind n with
    | Procdesc.Node.Start_node _ ->
        Int.equal (List.length succs) 0 || List.length preds > 0
    | Procdesc.Node.Exit_node _ ->
        List.length succs > 0 || Int.equal (List.length preds) 0
    | Procdesc.Node.Stmt_node _ | Procdesc.Node.Prune_node _ | Procdesc.Node.Skip_node _ ->
        Int.equal (List.length succs) 0 || Int.equal (List.length preds) 0
    | Procdesc.Node.Join_node ->
      (* Join node has the exception that it may be without predecessors
         and pointing to an exit node *)
      (* if the if brances end with a return *)
      match succs with [n'] when is_exit_node n' -> false | _ -> Int.equal (List.length preds) 0
  in
  not (List.exists ~f:is_broken_node (Procdesc.get_nodes proc_desc))


let load_statement =
  ResultsDatabase.register_statement "SELECT cfgs FROM source_files WHERE source_file = :k"


module SQLite = SqliteUtils.MarshalledData (struct
  type nonrec t = t
end)

let load source =
  ResultsDatabase.with_registered_statement load_statement ~f:(fun db load_stmt ->
      SourceFile.SQLite.serialize source |> Sqlite3.bind load_stmt 1
      |> SqliteUtils.check_sqlite_error db ~log:"load bind source file" ;
      SqliteUtils.sqlite_result_step ~finalize:false ~log:"Cfg.load" db load_stmt
      |> Option.map ~f:SQLite.deserialize )


let save_attributes source_file cfg =
  let save_proc _ pdesc =
    let attributes = Procdesc.get_attributes pdesc in
    let loc = attributes.loc in
    let attributes' =
      let loc' = if Location.equal loc Location.dummy then {loc with file= source_file} else loc in
      {attributes with loc= loc'; source_file_captured= source_file}
    in
    Attributes.store attributes'
  in
  Typ.Procname.Hash.iter save_proc cfg


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
      , [(* getter for fields *) (e1, _)] ) ->
        let instr' = Sil.Load (ret_id, Exp.Lfield (e1, fn, ft), bt, loc_call) in
        found instr instr'
    | Sil.Load (_, Exp.Lfield (Exp.Lvar pvar, fn, ft), bt, _), Some (ret_id, _), []
      when Pvar.is_global pvar ->
        (* getter for static fields *)
        let instr' = Sil.Load (ret_id, Exp.Lfield (Exp.Lvar pvar, fn, ft), bt, loc_call) in
        found instr instr'
    | Sil.Store (Exp.Lfield (_, fn, ft), bt, _, _), _, [(* setter for fields *) (e1, _); (e2, _)] ->
        let instr' = Sil.Store (Exp.Lfield (e1, fn, ft), bt, e2, loc_call) in
        found instr instr'
    | Sil.Store (Exp.Lfield (Exp.Lvar pvar, fn, ft), bt, _, _), _, [(e1, _)]
      when Pvar.is_global pvar ->
        (* setter for static fields *)
        let instr' = Sil.Store (Exp.Lfield (Exp.Lvar pvar, fn, ft), bt, e1, loc_call) in
        found instr instr'
    | Sil.Call (ret_id', Exp.Const Const.Cfun pn, etl', _, cf), _, _
      when Bool.equal (is_none ret_id) (is_none ret_id')
           && Int.equal (List.length etl') (List.length etl) ->
        let instr' = Sil.Call (ret_id, Exp.Const (Const.Cfun pn), etl, loc_call, cf) in
        found instr instr'
    | Sil.Call (ret_id', Exp.Const Const.Cfun pn, etl', _, cf), _, _
      when Bool.equal (is_none ret_id) (is_none ret_id')
           && Int.equal (List.length etl' + 1) (List.length etl) ->
        let etl1 =
          match List.rev etl with
          (* remove last element *)
          | _ :: l ->
              List.rev l
          | [] ->
              assert false
        in
        let instr' = Sil.Call (ret_id, Exp.Const (Const.Cfun pn), etl1, loc_call, cf) in
        found instr instr'
    | _ ->
        ()
  in
  Procdesc.iter_instrs do_instr pdesc ;
  !modified


(** Find synthetic (access or bridge) Java methods in the procedure and inline them in the cfg. *)
let proc_inline_synthetic_methods cfg pdesc : unit =
  let instr_inline_synthetic_method = function
    | Sil.Call (ret_id, Exp.Const Const.Cfun (Typ.Procname.Java java_pn as pn), etl, loc, _) -> (
      match Typ.Procname.Hash.find cfg pn with
      | pd ->
          let is_access = Typ.Procname.Java.is_access_method java_pn in
          let attributes = Procdesc.get_attributes pd in
          let is_synthetic = attributes.is_synthetic_method in
          let is_bridge = attributes.is_bridge_method in
          if is_access || is_bridge || is_synthetic then inline_synthetic_method ret_id etl pd loc
          else None
      | exception Not_found ->
          None )
    | _ ->
        None
  in
  let node_inline_synthetic_methods node =
    let modified = ref false in
    let do_instr instr =
      match instr_inline_synthetic_method instr with
      | None ->
          instr
      | Some instr' ->
          modified := true ;
          instr'
    in
    let instrs = Procdesc.Node.get_instrs node in
    let instrs' = List.map ~f:do_instr instrs in
    if !modified then Procdesc.Node.replace_instrs node instrs'
  in
  Procdesc.iter_nodes node_inline_synthetic_methods pdesc


let inline_java_synthetic_methods cfg =
  let f pname pdesc = if Typ.Procname.is_java pname then proc_inline_synthetic_methods cfg pdesc in
  Typ.Procname.Hash.iter f cfg


let pp_proc_signatures fmt cfg =
  F.fprintf fmt "METHOD SIGNATURES@\n@." ;
  let sorted_procs = List.sort ~cmp:Procdesc.compare (get_all_proc_descs cfg) in
  List.iter ~f:(fun pdesc -> F.fprintf fmt "%a@." Procdesc.pp_signature pdesc) sorted_procs
