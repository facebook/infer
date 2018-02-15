(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd
module L = Logging
module F = Format
module MF = MarkupFormatter

let desc_retain_cycle tenv (cycle: RetainCyclesType.t) =
  let open RetainCyclesType in
  Logging.d_strln "Proposition with retain cycle:" ;
  let do_edge index_ edge =
    let index = index_ + 1 in
    let node = State.get_node () in
    let from_exp_str edge_obj =
      match Errdesc.find_outermost_dereference tenv node edge_obj.rc_from.rc_node_exp with
      | Some de ->
          DecompiledExp.to_string de
      | None ->
          Format.sprintf "(object of type %s)" (Typ.to_string edge_obj.rc_from.rc_node_typ)
    in
    let location_str =
      match edge with
      | Object obj ->
          let update_str_list =
            Localise.access_desc (Errdesc.access_opt obj.rc_field.rc_field_inst)
          in
          if List.is_empty update_str_list then " "
          else ", " ^ String.concat ~sep:"," update_str_list
      | Block _ ->
          ""
    in
    let cycle_item_str =
      match edge with
      | Object obj ->
          MF.monospaced_to_string
            (Format.sprintf "%s->%s" (from_exp_str obj)
               (Typ.Fieldname.to_string obj.rc_field.rc_field_name))
      | Block _ ->
          Format.sprintf "a block"
    in
    Format.sprintf "(%d) %s%s" index cycle_item_str location_str
  in
  let cycle_str = List.mapi ~f:do_edge cycle.rc_elements in
  List.fold_left cycle_str ~f:(fun acc s -> Format.sprintf "%s\n %s" acc s) ~init:""


let edge_is_strong tenv obj_edge =
  let open RetainCyclesType in
  (* returns items annotation for field fn in struct t *)
  let get_item_annotation (t: Typ.t) fn =
    match t.desc with
    | Tstruct name
      -> (
        let equal_fn (fn', _, _) = Typ.Fieldname.equal fn fn' in
        match Tenv.lookup tenv name with
        | Some {fields; statics} ->
            let find fields =
              List.find ~f:equal_fn fields |> Option.value_map ~f:trd3 ~default:[]
            in
            find fields @ find statics
        | None ->
            [] )
    | _ ->
        []
  in
  let has_weak_or_unretained_or_assign params =
    List.exists
      ~f:(fun att ->
        String.equal Config.unsafe_unret att || String.equal Config.weak att
        || String.equal Config.assign att )
      params
  in
  let ia = get_item_annotation obj_edge.rc_from.rc_node_typ obj_edge.rc_field.rc_field_name in
  let weak_edge =
    List.exists
      ~f:(fun ((ann: Annot.t), _) ->
        ( String.equal ann.class_name Config.property_attributes
        || String.equal ann.class_name Config.ivar_attributes )
        && has_weak_or_unretained_or_assign ann.parameters )
      ia
  in
  not weak_edge


let get_cycle_blocks root_node exp =
  match exp with
  | Exp.Closure {name; captured_vars} ->
      List.find
        ~f:(fun (e, _, typ) ->
          match typ.Typ.desc with
          | Typ.Tptr (_, Typ.Pk_objc_weak) | Typ.Tptr (_, Typ.Pk_objc_unsafe_unretained) ->
              false
          | _ ->
              Exp.equal e root_node.RetainCyclesType.rc_node_exp )
        captured_vars
      |> Option.map ~f:(fun (_, var, _) -> (name, var))
  | _ ->
      None


let get_cycle root tenv prop =
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
  let rec dfs ~root_node ~from_node ~rev_path ~fields ~visited =
    match fields with
    | [] ->
        (rev_path, false)
    | (field, Sil.Eexp (f_exp, f_inst)) :: el' ->
        let rc_field = {rc_field_name= field; rc_field_inst= f_inst} in
        let obj_edge = {rc_from= from_node; rc_field} in
        let edge = Object obj_edge in
        (* found root, finish the cycle *)
        if edge_is_strong tenv obj_edge && Exp.equal f_exp root_node.rc_node_exp then
          (edge :: rev_path, true) (* we already visited f_exp, stop *)
        else if List.mem ~equal:Exp.equal visited f_exp then (rev_path, false)
        else
          let visited' = from_node.rc_node_exp :: visited in
          let cycle_block_opt = get_cycle_blocks root_node f_exp in
          (* cycle with a block *)
          if edge_is_strong tenv obj_edge && Option.is_some cycle_block_opt then
            let procname, var = Option.value_exn cycle_block_opt in
            (* From the captured variables we get the actual name of the variable
            that is more useful for the error message *)
            let updated_from_node = {from_node with rc_node_exp= Exp.Lvar var} in
            let edge = Object {obj_edge with rc_from= updated_from_node} in
            let edge2 = Block procname in
            (edge2 :: edge :: rev_path, true)
          else
            let res =
              match get_points_to f_exp with
              | None ->
                  (rev_path, false)
              | Some Sil.Hpointsto (_, Sil.Estruct (new_fields, _), Exp.Sizeof {typ= te})
                when edge_is_strong tenv obj_edge ->
                  let rc_to = {rc_node_exp= f_exp; rc_node_typ= te} in
                  dfs ~root_node ~from_node:rc_to ~rev_path:(edge :: rev_path) ~fields:new_fields
                    ~visited:visited'
              | _ ->
                  (rev_path, false)
            in
            if snd res then res
            else dfs ~root_node ~from_node ~rev_path ~fields:el' ~visited:visited'
    | _ ->
        (rev_path, false)
  in
  L.d_strln "Looking for cycle with root expression: " ;
  Sil.d_hpred root ;
  L.d_strln "" ;
  match root with
  | Sil.Hpointsto (e_root, Sil.Estruct (fl, _), Exp.Sizeof {typ= te}) ->
      let se_root = {rc_node_exp= e_root; rc_node_typ= te} in
      (* start dfs with empty path and expr pointing to root *)
      let pot_cycle, res =
        dfs ~root_node:se_root ~from_node:se_root ~rev_path:[] ~fields:fl ~visited:[]
      in
      if res then List.rev pot_cycle
      else (
        L.d_strln "NO cycle found from root" ;
        [] )
  | _ ->
      L.d_strln "Root exp is not an allocated object. No cycle found" ;
      []


let get_var_retain_cycle hpred tenv prop_ =
  (* returns the pvars of the first cycle we find in sigma.
     This is an heuristic that works if there is one cycle.
     In case there are more than one cycle we may return not necessarily
     the one we are looking for. *)
  let cycle_elements = get_cycle hpred tenv prop_ in
  RetainCyclesType.create_cycle cycle_elements


let exn_retain_cycle tenv hpred cycle =
  let retain_cycle = desc_retain_cycle tenv cycle in
  let cycle_dotty = Format.asprintf "%a" RetainCyclesType.pp_dotty cycle in
  ( if Config.debug_mode then
      let rc_dotty_dir = Filename.concat Config.results_dir Config.retain_cycle_dotty_dir in
      Utils.create_dir rc_dotty_dir ;
      let rc_dotty_file = Filename.temp_file ~in_dir:rc_dotty_dir "rc" ".dot" in
      RetainCyclesType.write_dotty_to_file rc_dotty_file cycle ) ;
  let desc = Localise.desc_retain_cycle retain_cycle (State.get_loc ()) (Some cycle_dotty) in
  Exceptions.Retain_cycle (hpred, desc, __POS__)


let report_cycle tenv hpred original_prop =
  (* When there is a cycle in objc we ignore it
        only if it's empty or it has weak or unsafe_unretained fields.
        Otherwise we report a retain cycle. *)
  let remove_opt prop_ = match prop_ with Some Some p -> p | _ -> Prop.prop_emp in
  let prop = remove_opt original_prop in
  match get_var_retain_cycle hpred tenv prop with
  | Some cycle ->
      RetainCyclesType.print_cycle cycle ;
      Some (exn_retain_cycle tenv hpred cycle)
  | _ ->
      None
