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

(** Classify bugs into buckets *)

module L = Logging
module F = Format

let verbose = Config.trace_error

(** check if the error was reported inside a nested loop
    the implementation is approximate: check if the last two visits to a loop were entering loops *)
let check_nested_loop path pos_opt =
  let trace_length = ref 0 in
  let loop_visits_log = ref [] in
  let in_nested_loop () =
    match !loop_visits_log with
    | true :: true :: _
     -> if verbose then L.d_strln "in nested loop" ;
        true
        (* last two loop visits were entering loops *)
    | _
     -> false
  in
  let do_node_caller node =
    match Procdesc.Node.get_kind node with
    | Procdesc.Node.Prune_node (b, (Sil.Ik_dowhile | Sil.Ik_for | Sil.Ik_while), _)
     -> (* if verbose then *)
        (*   L.d_strln ((if b then "enter" else "exit") ^ " node " *)
        (*              ^ (string_of_int (Procdesc.Node.get_id node))); *)
        loop_visits_log := b :: !loop_visits_log
    | _
     -> ()
  in
  let do_any_node _level _node =
    incr trace_length
    (* L.d_strln *)
    (*   ("level " ^ string_of_int _level ^ *)
    (*    " (Procdesc.Node.get_id node) " ^ string_of_int (Procdesc.Node.get_id _node)) *)
  in
  let f level p _ _ =
    match Paths.Path.curr_node p with
    | Some node
     -> do_any_node level node ;
        if Int.equal level 0 then do_node_caller node
    | None
     -> ()
  in
  Paths.Path.iter_shortest_sequence f pos_opt path ; in_nested_loop ()

(** Check that we know where the value was last assigned,
    and that there is a local access instruction at that line. **)
let check_access access_opt de_opt =
  let find_bucket line_number null_case_flag =
    let find_formal_ids node =
      (* find ids obtained by a letref on a formal parameter *)
      let node_instrs = Procdesc.Node.get_instrs node in
      let formals =
        match State.get_prop_tenv_pdesc () with
        | None
         -> []
        | Some (_, _, pdesc)
         -> Procdesc.get_formals pdesc
      in
      let formal_names = List.map ~f:fst formals in
      let is_formal pvar =
        let name = Pvar.get_name pvar in
        List.exists ~f:(Mangled.equal name) formal_names
      in
      let formal_ids = ref [] in
      let process_formal_letref = function
        | Sil.Load (id, Exp.Lvar pvar, _, _)
         -> let is_java_this = Config.curr_language_is Config.Java && Pvar.is_this pvar in
            if not is_java_this && is_formal pvar then formal_ids := id :: !formal_ids
        | _
         -> ()
      in
      List.iter ~f:process_formal_letref node_instrs ; !formal_ids
    in
    let formal_param_used_in_call = ref false in
    let has_call_or_sets_null node =
      let rec exp_is_null exp =
        match exp with
        | Exp.Const Const.Cint n
         -> IntLit.iszero n
        | Exp.Cast (_, e)
         -> exp_is_null e
        | Exp.Var _ | Exp.Lvar _ -> (
          match State.get_const_map () node exp with
          | Some Const.Cint n
           -> IntLit.iszero n
          | _
           -> false )
        | _
         -> false
      in
      let filter = function
        | Sil.Call (_, _, etl, _, _)
         -> let formal_ids = find_formal_ids node in
            let arg_is_formal_param (e, _) =
              match e with Exp.Var id -> List.exists ~f:(Ident.equal id) formal_ids | _ -> false
            in
            if List.exists ~f:arg_is_formal_param etl then formal_param_used_in_call := true ;
            true
        | Sil.Store (_, _, e, _)
         -> exp_is_null e
        | _
         -> false
      in
      List.exists ~f:filter (Procdesc.Node.get_instrs node)
    in
    let local_access_found = ref false in
    let do_node node =
      if Int.equal (Procdesc.Node.get_loc node).Location.line line_number
         && has_call_or_sets_null node
      then local_access_found := true
    in
    let path, pos_opt = State.get_path () in
    Paths.Path.iter_all_nodes_nocalls do_node path ;
    if !local_access_found then
      let bucket =
        if null_case_flag then Localise.BucketLevel.b5
        else if check_nested_loop path pos_opt then Localise.BucketLevel.b3
        else if !formal_param_used_in_call then Localise.BucketLevel.b2
        else Localise.BucketLevel.b1
      in
      Some bucket
    else None
  in
  match access_opt with
  | Some Localise.Last_assigned (n, ncf)
   -> find_bucket n ncf
  | Some Localise.Returned_from_call n
   -> find_bucket n false
  | Some Localise.Last_accessed (_, is_nullable) when is_nullable
   -> Some Localise.BucketLevel.b1
  | _ ->
    match de_opt with Some DecompiledExp.Dconst _ -> Some Localise.BucketLevel.b1 | _ -> None

let classify_access desc access_opt de_opt is_nullable =
  let default_bucket = if is_nullable then Localise.BucketLevel.b1 else Localise.BucketLevel.b5 in
  let show_in_message = Config.show_buckets in
  match check_access access_opt de_opt with
  | None
   -> Localise.error_desc_set_bucket desc default_bucket show_in_message
  | Some bucket
   -> Localise.error_desc_set_bucket desc bucket show_in_message
