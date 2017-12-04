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

let get_cycle root prop =
  let sigma = prop.Prop.sigma in
  let get_points_to e =
    match e with
    | Sil.Eexp (e', _) ->
        List.find
          ~f:(fun hpred ->
            match hpred with Sil.Hpointsto (e'', _, _) -> Exp.equal e'' e' | _ -> false)
          sigma
    | _ ->
        None
  in
  let print_cycle cyc =
    L.d_str "Cycle= " ;
    List.iter
      ~f:(fun ((e, t), f, e') ->
        match (e, e') with
        | Sil.Eexp (e, _), Sil.Eexp (e', _) ->
            L.d_str
              ( "(" ^ Exp.to_string e ^ ": " ^ Typ.to_string t ^ ", " ^ Typ.Fieldname.to_string f
              ^ ", " ^ Exp.to_string e' ^ ")" )
        | _ ->
            ())
      cyc ;
    L.d_strln ""
  in
  (* Perform a dfs of a graph stopping when e_root is reached.
     Returns a pair (path, bool) where path is a list of edges ((e1,type_e1),f,e2)
     describing the path to e_root and bool is true if e_root is reached. *)
  let rec dfs e_root et_src path el visited =
    match el with
    | [] ->
        (path, false)
    | (f, e) :: el' ->
        if Sil.equal_strexp e e_root then ((et_src, f, e) :: path, true)
        else if List.mem ~equal:Sil.equal_strexp visited e then (path, false)
        else
          let visited' = fst et_src :: visited in
          let res =
            match get_points_to e with
            | None ->
                (path, false)
            | Some Sil.Hpointsto (_, Sil.Estruct (fl, _), Exp.Sizeof {typ= te}) ->
                dfs e_root (e, te) ((et_src, f, e) :: path) fl visited'
            | _ ->
                (path, false)
            (* check for lists *)
          in
          if snd res then res else dfs e_root et_src path el' visited'
  in
  L.d_strln "Looking for cycle with root expression: " ;
  Sil.d_hpred root ;
  L.d_strln "" ;
  match root with
  | Sil.Hpointsto (e_root, Sil.Estruct (fl, _), Exp.Sizeof {typ= te}) ->
      let se_root = Sil.Eexp (e_root, Sil.Inone) in
      (* start dfs with empty path and expr pointing to root *)
      let pot_cycle, res = dfs se_root (se_root, te) [] fl [] in
      if res then ( print_cycle pot_cycle ; pot_cycle )
      else (
        L.d_strln "NO cycle found from root" ;
        [] )
  | _ ->
      L.d_strln "Root exp is not an allocated object. No cycle found" ;
      []


let get_retain_cycle_dotty prop_ cycle =
  match prop_ with
  | None ->
      None
  | Some Some prop_ ->
      Dotty.dotty_prop_to_str prop_ cycle
  | _ ->
      None


let get_var_retain_cycle prop_ =
  let sigma = prop_.Prop.sigma in
  let is_pvar v h =
    match h with
    | Sil.Hpointsto (Exp.Lvar _, v', _) when Sil.equal_strexp v v' ->
        true
    | _ ->
        false
  in
  let is_hpred_block v h =
    match (h, v) with
    | Sil.Hpointsto (e, _, Exp.Sizeof {typ}), Sil.Eexp (e', _)
      when Exp.equal e e' && Typ.is_block_type typ ->
        true
    | _, _ ->
        false
  in
  let find v = List.find ~f:(is_pvar v) sigma |> Option.map ~f:Sil.hpred_get_lhs in
  let find_block v =
    if List.exists ~f:(is_hpred_block v) sigma then Some (Exp.Lvar Sil.block_pvar) else None
  in
  let sexp e = Sil.Eexp (e, Sil.Inone) in
  let find_or_block ((e, t), f, e') =
    match find e with
    | Some pvar ->
        [((sexp pvar, t), f, e')]
    | _ ->
      match find_block e with
      | Some blk ->
          [((sexp blk, t), f, e')]
      | _ ->
          let sizeof = {Exp.typ= t; nbytes= None; dynamic_length= None; subtype= Subtype.exact} in
          [((sexp (Exp.Sizeof sizeof), t), f, e')]
  in
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
        L.d_strln "Filtering pvar in cycle " ;
        let cycle' = List.concat_map ~f:find_or_block cycle in
        if List.is_empty cycle' then do_sigma sigma' else cycle'
  in
  do_sigma sigma


(** Checks if cycle has fields (derived from a property or directly defined as ivar) with attributes
    weak/unsafe_unretained/assing *)
let cycle_has_weak_or_unretained_or_assign_field tenv cycle =
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
    match c with
    | [] ->
        false
    | ((_, t), fn, _) :: c' ->
        let ia = get_item_annotation t fn in
        if List.exists ~f:do_annotation ia then true else do_cycle c'
  in
  do_cycle cycle


let exn_retain_cycle original_prop hpred cycle =
  let cycle_dotty = get_retain_cycle_dotty original_prop cycle in
  let desc = Errdesc.explain_retain_cycle cycle (State.get_loc ()) cycle_dotty in
  Exceptions.Retain_cycle (hpred, desc, __POS__)


let report_cycle tenv hpred original_prop =
  (* When there is a cycle in objc we ignore it
        only if it's empty or it has weak or unsafe_unretained fields.
        Otherwise we report a retain cycle. *)
  let remove_opt prop_ = match prop_ with Some Some p -> p | _ -> Prop.prop_emp in
  let cycle = get_var_retain_cycle (remove_opt original_prop) in
  let ignore_cycle =
    Int.equal (List.length cycle) 0 || cycle_has_weak_or_unretained_or_assign_field tenv cycle
  in
  (ignore_cycle, exn_retain_cycle original_prop hpred cycle)

