(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module F = Format

(** data type for the control flow graph *)
type t = Procdesc.t Typ.Procname.Hash.t

let create () = Typ.Procname.Hash.create 16

let iter_over_sorted_procs cfg ~f =
  let compare_proc_desc_by_proc_name pdesc1 pdesc2 =
    Typ.Procname.compare (Procdesc.get_proc_name pdesc1) (Procdesc.get_proc_name pdesc2)
  in
  Typ.Procname.Hash.fold (fun _ pdesc acc -> pdesc :: acc) cfg []
  |> List.sort ~compare:compare_proc_desc_by_proc_name
  |> List.iter ~f


let get_all_defined_proc_names cfg =
  let procs = ref [] in
  let f pname pdesc = if Procdesc.is_defined pdesc then procs := pname :: !procs in
  Typ.Procname.Hash.iter f cfg ; !procs


(** Create a new procdesc *)
let create_proc_desc cfg (proc_attributes : ProcAttributes.t) =
  let pdesc = Procdesc.from_proc_attributes proc_attributes in
  Typ.Procname.Hash.add cfg proc_attributes.proc_name pdesc ;
  pdesc


let iter_sorted cfg ~f = iter_over_sorted_procs cfg ~f

let store source_file cfg =
  let save_proc _ proc_desc =
    let attributes = Procdesc.get_attributes proc_desc in
    let loc = attributes.loc in
    let attributes' =
      let loc' = if Location.equal loc Location.dummy then {loc with file= source_file} else loc in
      {attributes with loc= loc'; translation_unit= source_file}
    in
    Procdesc.set_attributes proc_desc attributes' ;
    Attributes.store ~proc_desc:(Some proc_desc) attributes'
  in
  Typ.Procname.Hash.iter save_proc cfg


(** Inline a synthetic (access or bridge) method. *)
let inline_synthetic_method ((ret_id, _) as ret) etl pdesc loc_call : Sil.instr option =
  let found instr instr' =
    L.(debug Analysis Verbose)
      "XX inline_synthetic_method found instr: %a@."
      (Sil.pp_instr ~print_types:true Pp.text)
      instr ;
    L.(debug Analysis Verbose)
      "XX inline_synthetic_method instr': %a@."
      (Sil.pp_instr ~print_types:true Pp.text)
      instr' ;
    Some instr'
  in
  let do_instr instr =
    match (instr, etl) with
    | Sil.Load (_, Exp.Lfield (Exp.Var _, fn, ft), bt, _), [(* getter for fields *) (e1, _)] ->
        let instr' = Sil.Load (ret_id, Exp.Lfield (e1, fn, ft), bt, loc_call) in
        found instr instr'
    | Sil.Load (_, Exp.Lfield (Exp.Lvar pvar, fn, ft), bt, _), [] when Pvar.is_global pvar ->
        (* getter for static fields *)
        let instr' = Sil.Load (ret_id, Exp.Lfield (Exp.Lvar pvar, fn, ft), bt, loc_call) in
        found instr instr'
    | Sil.Store (Exp.Lfield (_, fn, ft), bt, _, _), [(* setter for fields *) (e1, _); (e2, _)] ->
        let instr' = Sil.Store (Exp.Lfield (e1, fn, ft), bt, e2, loc_call) in
        found instr instr'
    | Sil.Store (Exp.Lfield (Exp.Lvar pvar, fn, ft), bt, _, _), [(e1, _)] when Pvar.is_global pvar
      ->
        (* setter for static fields *)
        let instr' = Sil.Store (Exp.Lfield (Exp.Lvar pvar, fn, ft), bt, e1, loc_call) in
        found instr instr'
    | Sil.Call (_, Exp.Const (Const.Cfun pn), etl', _, cf), _
      when Int.equal (List.length etl') (List.length etl) ->
        let instr' = Sil.Call (ret, Exp.Const (Const.Cfun pn), etl, loc_call, cf) in
        found instr instr'
    | Sil.Call (_, Exp.Const (Const.Cfun pn), etl', _, cf), _
      when Int.equal (List.length etl' + 1) (List.length etl) ->
        let etl1 =
          match List.rev etl with
          (* remove last element *)
          | _ :: l ->
              List.rev l
          | [] ->
              assert false
        in
        let instr' = Sil.Call (ret, Exp.Const (Const.Cfun pn), etl1, loc_call, cf) in
        found instr instr'
    | _ ->
        None
  in
  Procdesc.find_map_instrs ~f:do_instr pdesc


(** Find synthetic (access or bridge) Java methods in the procedure and inline them in the cfg. *)
let proc_inline_synthetic_methods cfg pdesc : unit =
  let instr_inline_synthetic_method _node instr =
    match instr with
    | Sil.Call (ret_id_typ, Exp.Const (Const.Cfun (Typ.Procname.Java java_pn as pn)), etl, loc, _)
      -> (
      match Typ.Procname.Hash.find cfg pn with
      | pd ->
          let is_access = Typ.Procname.Java.is_access_method java_pn in
          let attributes = Procdesc.get_attributes pd in
          let is_synthetic = attributes.is_synthetic_method in
          let is_bridge = attributes.is_bridge_method in
          if is_access || is_bridge || is_synthetic then
            inline_synthetic_method ret_id_typ etl pd loc |> Option.value ~default:instr
          else instr
      | exception Caml.Not_found ->
          instr )
    | _ ->
        instr
  in
  let (_updated : bool) = Procdesc.replace_instrs pdesc ~f:instr_inline_synthetic_method in
  ()


let inline_java_synthetic_methods cfg =
  let f pname pdesc = if Typ.Procname.is_java pname then proc_inline_synthetic_methods cfg pdesc in
  Typ.Procname.Hash.iter f cfg


let pp_proc_signatures fmt cfg =
  F.fprintf fmt "@[<v>METHOD SIGNATURES@;" ;
  iter_over_sorted_procs ~f:(Procdesc.pp_signature fmt) cfg ;
  F.fprintf fmt "@]"
