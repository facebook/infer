(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
module L = Logging
module F = Format
module MF = MarkupFormatter

let desc_retain_cycle tenv (cycle: RetainCyclesType.t) =
  let open RetainCyclesType in
  Logging.d_strln "Proposition with retain cycle:" ;
  let do_edge index_ edge =
    let index = index_ + 1 in
    let node = State.get_node () in
    let from_exp_str =
      match edge.rc_from.rc_node_exp with
      | Exp.Lvar pvar when Pvar.equal pvar Sil.block_pvar ->
          "a block"
      | _ ->
        match Errdesc.find_outermost_dereference tenv node edge.rc_from.rc_node_exp with
        | Some de ->
            DecompiledExp.to_string de
        | None ->
            Format.sprintf "(object of type %s)" (Typ.to_string edge.rc_from.rc_node_typ)
    in
    let inst_str inst =
      let update_str_list = Localise.access_desc (ref []) (Errdesc.access_opt inst) in
      if List.is_empty update_str_list then " " else ", " ^ String.concat ~sep:"," update_str_list
    in
    let cycle_item_str =
      Format.sprintf "%s->%s" from_exp_str (Typ.Fieldname.to_string edge.rc_field.rc_field_name)
    in
    Format.sprintf "(%d) %s%s" index
      (MF.monospaced_to_string cycle_item_str)
      (inst_str edge.rc_field.rc_field_inst)
  in
  let cycle_str = List.mapi ~f:do_edge cycle.rc_elements in
  List.fold_left cycle_str ~f:(fun acc s -> Format.sprintf "%s\n %s" acc s) ~init:""


let get_cycle root prop =
  let open RetainCyclesType in
  let sigma = prop.Prop.sigma in
  let get_points_to e =
    List.find
      ~f:(fun hpred -> match hpred with Sil.Hpointsto (e', _, _) -> Exp.equal e' e | _ -> false)
      sigma
  in
  (* Perform a dfs of a graph stopping when e_root is reached.
   Returns a pair (path, bool) where path is a list of edges 
   describing the path to e_root and bool is true if e_root is reached. *)
  let rec dfs root_node from_node path fields visited =
    match fields with
    | [] ->
        (path, false)
    | (field, Sil.Eexp (f_exp, f_inst)) :: el' ->
        if Exp.equal f_exp root_node.rc_node_exp then
          let rc_field = {rc_field_name= field; rc_field_inst= f_inst} in
          let edge = {rc_from= from_node; rc_field} in
          (edge :: path, true)
        else if List.mem ~equal:Exp.equal visited f_exp then (path, false)
        else
          let visited' = from_node.rc_node_exp :: visited in
          let res =
            match get_points_to f_exp with
            | None ->
                (path, false)
            | Some Sil.Hpointsto (_, Sil.Estruct (new_fields, _), Exp.Sizeof {typ= te}) ->
                let rc_field = {rc_field_name= field; rc_field_inst= f_inst} in
                let edge = {rc_from= from_node; rc_field} in
                let rc_to = {rc_node_exp= f_exp; rc_node_typ= te} in
                dfs root_node rc_to (edge :: path) new_fields visited'
            | _ ->
                (path, false)
            (* check for lists *)
          in
          if snd res then res else dfs root_node from_node path el' visited'
    | _ ->
        (path, false)
  in
  L.d_strln "Looking for cycle with root expression: " ;
  Sil.d_hpred root ;
  L.d_strln "" ;
  match root with
  | Sil.Hpointsto (e_root, Sil.Estruct (fl, _), Exp.Sizeof {typ= te}) ->
      let se_root = {rc_node_exp= e_root; rc_node_typ= te} in
      (* start dfs with empty path and expr pointing to root *)
      let pot_cycle, res = dfs se_root se_root [] fl [] in
      if res then pot_cycle
      else (
        L.d_strln "NO cycle found from root" ;
        [] )
  | _ ->
      L.d_strln "Root exp is not an allocated object. No cycle found" ;
      []


let get_var_retain_cycle prop_ =
  let sigma = prop_.Prop.sigma in
  (* returns the pvars of the first cycle we find in sigma.
     This is an heuristic that works if there is one cycle.
     In case there are more than one cycle we may return not necessarily
     the one we are looking for. *)
  let rec do_sigma sigma_todo =
    match sigma_todo with
    | [] ->
        []
    | hp :: sigma' ->
        let cycle = get_cycle hp prop_ in
        if List.is_empty cycle then do_sigma sigma' else cycle
  in
  let cycle_elements = do_sigma sigma in
  RetainCyclesType.create_cycle cycle_elements


(** Checks if cycle has fields (derived from a property or directly defined as ivar) with attributes
    weak/unsafe_unretained/assing *)
let cycle_has_weak_or_unretained_or_assign_field tenv cycle =
  let open RetainCyclesType in
  (* returns items annotation for field fn in struct t *)
  let get_item_annotation (t: Typ.t) fn =
    match t.desc with
    | Tstruct name
      -> (
        let equal_fn (fn', _, _) = Typ.Fieldname.equal fn fn' in
        match Tenv.lookup tenv name with
        | Some {fields; statics} ->
            List.find ~f:equal_fn (fields @ statics) |> Option.value_map ~f:trd3 ~default:[]
        | None ->
            [] )
    | _ ->
        []
  in
  let rec has_weak_or_unretained_or_assign params =
    match params with
    | [] ->
        false
    | att :: _
      when String.equal Config.unsafe_unret att || String.equal Config.weak att
           || String.equal Config.assign att ->
        true
    | _ :: params' ->
        has_weak_or_unretained_or_assign params'
  in
  let do_annotation ((a: Annot.t), _) =
    ( String.equal a.class_name Config.property_attributes
    || String.equal a.class_name Config.ivar_attributes )
    && has_weak_or_unretained_or_assign a.parameters
  in
  let rec do_cycle c =
    let open RetainCyclesType in
    match c with
    | [] ->
        false
    | edge :: c' ->
        let ia = get_item_annotation edge.rc_from.rc_node_typ edge.rc_field.rc_field_name in
        if List.exists ~f:do_annotation ia then true else do_cycle c'
  in
  do_cycle cycle.rc_elements


let exn_retain_cycle tenv prop hpred cycle =
  let retain_cycle = desc_retain_cycle tenv cycle in
  let cycle_dotty = Dotty.dotty_retain_cycle_to_str prop cycle in
  let desc = Localise.desc_retain_cycle retain_cycle (State.get_loc ()) cycle_dotty in
  Exceptions.Retain_cycle (hpred, desc, __POS__)


let report_cycle tenv hpred original_prop =
  (* When there is a cycle in objc we ignore it
        only if it's empty or it has weak or unsafe_unretained fields.
        Otherwise we report a retain cycle. *)
  let remove_opt prop_ = match prop_ with Some Some p -> p | _ -> Prop.prop_emp in
  let prop = remove_opt original_prop in
  match get_var_retain_cycle prop with
  | Some cycle when not (cycle_has_weak_or_unretained_or_assign_field tenv cycle) ->
      RetainCyclesType.print_cycle cycle ;
      Some (exn_retain_cycle tenv prop hpred cycle)
  | _ ->
      None

